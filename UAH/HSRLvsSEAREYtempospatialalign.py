import pandas as pd
import numpy as np
import h5py
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

import cartopy.crs as ccrs
import cartopy.feature as cfeature

# ============================================================
# USER SETTINGS
# ============================================================

SEAREY_FILE = r"D:\TEMPO3\rewritten_flights\SEAREY_20230802_R0_L20Z"
HSRL_FILE   = r"D:\TEMPO3\HSRL802\staqs-HSRL2_JSC-GV_20230802_R1.h5"

FLIGHT_DATE = "2023-08-02"

LAT_TOL  = 0.05        # degrees
LON_TOL  = 0.01          # degrees
TIME_TOL = pd.Timedelta("20min")  # temporal tolerance for colocation

SEAREY_O3_COL = "Ozone_ppbv"
ALT_BIN  = 15
TIME_BIN = "5min"         # SEAREY curtain time bin

# ============================================================
# LOAD SEAREY
# ============================================================

def load_searey(path):
    with open(path, "r", encoding="latin1") as f:
        lines = f.readlines()

    start_line = None
    for i, line in enumerate(lines):
        if line.strip().startswith("Time_Mid, Latitude"):
            start_line = i
            break

    df = pd.read_csv(
        path,
        skiprows=start_line,
        sep=",",
        skipinitialspace=True,
        encoding="latin1"
    )

    for col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

    df = df.dropna(how="all", subset=df.columns[1:])

    df["time"] = pd.to_datetime(FLIGHT_DATE) + pd.to_timedelta(df["Time_Mid"], unit="s")

    return df

# ============================================================
# LOAD HSRL 
# ============================================================

def load_hsrl_o3(path):
    with h5py.File(path, "r") as f:
        lat = np.squeeze(f["lat"][()])
        lon = np.squeeze(f["lon"][()])
        t_raw = np.squeeze(f["time"][()])  # seconds since midnight UTC

        o3  = np.squeeze(f["DataProducts/O3"][()])
        alt = np.squeeze(f["z"][()])

    t = pd.to_datetime(FLIGHT_DATE) + pd.to_timedelta(t_raw, unit="s")

    df = pd.DataFrame({
        "lat": lat,
        "lon": lon,
        "time_hsrl": t,
        "O3": [row.tolist() for row in o3],
        "alt": [alt.tolist()] * len(lat)
    })

    return df


# ============================================================
# STRICT SPATIOTEMPORAL MATCHING: HSRL ↔ SEAREY
# ============================================================

def match_hsrl_to_searey_time(df_searey, df_hsrl, lat_tol, lon_tol, time_tol):
    matched = []

    for _, s in df_searey.iterrows():
        lat_s, lon_s, t_s = s["Latitude"], s["Longitude"], s["time"]

        dlat = np.abs(df_hsrl["lat"] - lat_s)
        dlon = np.abs(df_hsrl["lon"] - lon_s)
        dt   = np.abs(df_hsrl["time_hsrl"] - t_s)

        mask = (dlat <= lat_tol) & (dlon <= lon_tol) & (dt <= time_tol)

        if mask.any():
            dist2 = dlat**2 + dlon**2
            idx = dist2[mask].idxmin()

            h = df_hsrl.loc[idx].copy()
            h["time"] = t_s   # project onto SEAREY time
            matched.append(h)

    if not matched:
        print("No matches found under current tolerances.")
        return None

    return pd.DataFrame(matched)

# ============================================================
# EXPAND HSRL PROFILES 
# ============================================================

def expand_hsrl_profiles(df, varname):
    rows = []

    for i, row in df.reset_index().iterrows():
        alt = row["alt"]
        prof = row[varname]
        t = row["time"]
        lat = row["lat"]
        lon = row["lon"]

        for a, v in zip(alt, prof):
            rows.append({
                "profile_id": i,
                "time": t,
                "lat": lat,
                "lon": lon,
                "Altitude_m": a,
                varname: v
            })

    return pd.DataFrame(rows)

# ============================================================
# BUILD CURTAIN ON SEAREY TIME GRID
# ============================================================

def build_curtain(df, varname, alt_bin, time_bin):
    df = df.copy()
    df["alt_bin"] = (df["Altitude_m"] // alt_bin) * alt_bin
    df["time_bin"] = df["time"].dt.floor(time_bin)

    curtain = df.pivot_table(
        index="alt_bin",
        columns="time_bin",
        values=varname,
        aggfunc="mean"
    )

    return curtain

# ============================================================
# PLOT FUSED CURTAIN
# ============================================================

def plot_fused_curtain(fused, title, vmin=None, vmax=None):
    plt.figure(figsize=(12, 6))

    C = fused.values
    ny, nx = C.shape

    x_edges = np.arange(nx + 1)

    y_centers = fused.index.values.astype(float)
    if ny > 1:
        dy = np.median(np.diff(y_centers))
    else:
        dy = ALT_BIN
    y_edges = np.concatenate(([y_centers[0] - dy / 2],
                              (y_centers[:-1] + y_centers[1:]) / 2,
                              [y_centers[-1] + dy / 2]))

    X, Y = np.meshgrid(x_edges, y_edges)

    pcm = plt.pcolormesh(
        X,
        Y,
        C,
        shading="flat",
        cmap="turbo",
        vmin=0,
        vmax=130
    )

    # Derive SEAREY times
    searey_cols = [c for c in fused.columns if str(c).endswith("_SEAREY")]
    searey_times = pd.to_datetime([str(c).replace("_SEAREY", "") for c in searey_cols])

    # --- FORMAT: convert to CST for display ---
    from datetime import timezone, timedelta
    CST = timezone(timedelta(hours=-6))
    searey_times_cst = searey_times.tz_localize("UTC").tz_convert(CST)


    # --- FORMAT: 30‑minute ticks only ---
    tick_positions = []
    tick_labels = []
    for i, t in enumerate(searey_times_cst):
        if t.minute % 30 == 0:
            tick_positions.append(0.5 + 2*i)
            tick_labels.append(t.strftime("%H:%M"))

    plt.xticks(tick_positions, tick_labels, rotation=45, fontsize=10)

    # --- FORMAT: axis labels small ---
    plt.xlabel("Time (SEAREY bins)", fontsize=12)
    plt.ylabel("Altitude (mASL)", fontsize=12)
    plt.ylim(0, 2000)

    # --- FORMAT: title large ---
    plt.title(title, fontsize=22)

    # --- FORMAT: colorbar small ---
    cbar = plt.colorbar(pcm)
    cbar.set_label("O₃ (ppbv) — SEAREY curtain + colocated HSRL", fontsize=12)
    cbar.ax.tick_params(labelsize=10)
    


    plt.tight_layout()
    plt.show()



# ============================================================
# SPATIAL DISPLAY ON CARTOPY MAP
# ============================================================
from datetime import timezone, timedelta
import zoneinfo

def plot_spatial_tracks(df_searey, df_match):
    proj = ccrs.PlateCarree()

    CST = timezone(timedelta(hours=-6))


    # Increase fonts globally
    plt.rcParams.update({
        "font.size": 16,
        "axes.titlesize": 20,
        "axes.labelsize": 18,
        "xtick.labelsize": 14,
        "ytick.labelsize": 14,
        "legend.fontsize": 14,
        "figure.titlesize": 22
    })

    fig = plt.figure(figsize=(10, 8))
    ax = plt.axes(projection=proj)

    ax.set_title("SEAREY and HSRL Flight Tracks with Strict Colocation")

    ax.add_feature(cfeature.COASTLINE, linewidth=0.7)
    ax.add_feature(cfeature.BORDERS, linewidth=0.7)
    ax.add_feature(cfeature.STATES, linewidth=0.5, edgecolor="gray")

    # SEAREY full track
    ax.plot(
        df_searey["Longitude"],
        df_searey["Latitude"],
        "-k",
        linewidth=1.0,
        transform=proj,
        label="SEAREY track"
    )

    # HSRL native track (subset that matched)
    ax.plot(
        df_match["lon"],
        df_match["lat"],
        marker="o", linestyle="none",
        linewidth=1.0,
        alpha=0.5,
        transform=proj,
        #label="HSRL track (colocated subset)"
    )

    # Colocated points colored by SEAREY time
    sc = ax.scatter(
        df_match["lon"],
        df_match["lat"],
        c=df_match["time"].map(mdates.date2num),
        cmap="turbo",
        s=80,
        edgecolor="k",
        zorder=5,
        transform=proj,
        label="Colocated samples"
    )

    # Colorbar in CST
    cbar = plt.colorbar(sc, ax=ax, orientation="vertical", pad=0.02)
    cbar.ax.set_ylabel("Colocated time (CST)")
    cbar.ax.yaxis.set_major_formatter(
        mdates.DateFormatter("%H:%M", tz=CST)
    )

    # Zoomed extent
    lon_min = min(df_searey["Longitude"].min(), df_match["lon"].min()) - 0.1
    lon_max = max(df_searey["Longitude"].max(), df_match["lon"].max()) + 0.1
    lat_min = min(df_searey["Latitude"].min(), df_match["lat"].min()) - 0.1
    lat_max = max(df_searey["Latitude"].max(), df_match["lat"].max()) + 0.1
    ax.set_extent([lon_min, lon_max, lat_min, lat_max], crs=proj)


    ax.legend(loc="lower left")
    plt.tight_layout()
    plt.show()


# ============================================================
# MAIN
# ============================================================

if __name__ == "__main__":

    print("Loading SEAREY...")
    df_searey = load_searey(SEAREY_FILE)

    print("Loading HSRL...")
    df_hsrl = load_hsrl_o3(HSRL_FILE)

    print("Matching HSRL to SEAREY with strict spatiotemporal criteria...")
    df_match = match_hsrl_to_searey_time(
        df_searey,
        df_hsrl,
        LAT_TOL,
        LON_TOL,
        TIME_TOL
    )

    if df_match is None:
        raise SystemExit("No matches found — try relaxing LAT_TOL, LON_TOL, or TIME_TOL.")

    print("Expanding HSRL profiles...")
    df_hsrl_long = expand_hsrl_profiles(df_match, "O3")

    print("Building SEAREY curtain (backbone)...")
    df_searey_long = df_searey.rename(columns={"Altitude_m_MSL": "Altitude_m"})
    searey_curtain = build_curtain(df_searey_long, SEAREY_O3_COL, ALT_BIN, TIME_BIN)
    searey_curtain = searey_curtain.add_suffix("_SEAREY")

    print("Building HSRL curtain on SEAREY time grid (colocated only)...")
    hsrl_curtain = build_curtain(df_hsrl_long, "O3", ALT_BIN, TIME_BIN)
    hsrl_curtain = hsrl_curtain.add_suffix("_HSRL")

    print("Fusing curtains...")

    # Align on altitude (rows)
    searey_curtain, hsrl_curtain = searey_curtain.align(hsrl_curtain, join="outer", axis=0)

    # For every SEAREY time bin, create a SEAREY column and an HSRL column.
    # If HSRL is missing for that time, insert an all-NaN HSRL column.
    paired_cols = []
    for searey_col in searey_curtain.columns:
        # Always keep the SEAREY column
        paired_cols.append(searey_curtain[searey_col])

        # Derive the matching HSRL column name
        t_str = str(searey_col).replace("_SEAREY", "")
        hsrl_col = f"{t_str}_HSRL"

        if hsrl_col in hsrl_curtain.columns:
            paired_cols.append(hsrl_curtain[hsrl_col])
        else:
            # Create an empty HSRL column for this time bin
            empty = pd.Series(
                np.nan,
                index=searey_curtain.index,
                name=hsrl_col
            )
            paired_cols.append(empty)

    fused = pd.concat(paired_cols, axis=1)


    print("Plotting fused curtain...")
    plot_fused_curtain(
        fused,
        f"SEAREY Curtain + Filtered Colocated HSRL O₃ ({TIME_BIN} bins)"
    )

    print("Plotting spatial tracks with colocated points...")
    plot_spatial_tracks(df_searey, df_match)
    
    print("SEAREY full:", df_searey["time"].min(), df_searey["time"].max())
    print("SEAREY binned:", searey_curtain.columns.min(), searey_curtain.columns.max())
    print("HSRL matched:", df_match["time"].min(), df_match["time"].max())


