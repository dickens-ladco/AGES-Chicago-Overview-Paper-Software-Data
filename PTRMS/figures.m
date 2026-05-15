%% load data

load("C:\Users\Martina\Downloads\AGES_R0_mnr (1).mat", "amb_1Hz")
load("F:\Chicago\amb_5Hz.mat")
load("F:\Chicago\Publications\AGES Chicago Paper\fig_a.mat")
load("F:\Chicago\Publications\AGES Chicago Paper\final_data.mat")

aromatics_1hz=amb_1Hz.benzene+amb_1Hz.xylene+amb_1Hz.TMB;
aromatics_5min=amb.benzene+amb.xylene+amb.TMB;

mt=monoterpenes(66:70, 1:10);
mt=mean(mt);
mt_array=table2array(mt);
arm=GC(66:70, [8 9 11]);
arm=mean(arm);
arm_array=table2array(arm);
%%
set(groot,'defaultLineLineWidth',2.0)
figure
tiledlayout(3,1)
nexttile(1)
pbaspect([4 1 1])
b=bar((uncal_groups_hourly.time+minutes(30)), uncal_groups_hourly_array,'stacked','BarWidth', 1)
ax=gca;
ylim([0 inf])
newColors = [
0.04, 0.42, 0.21;
0.21, 0.57, 0.28;
0.36, 0.75, 0.35;
0.68, 0.97, 0.72;
0.73, 0.51, 0.25;
0.64, 0.16, 0.08;
0.82, 0.82, 0.39;
0.87, 0.87, 0.87;
];
for i = 1:length(b)
b(i).FaceColor = newColors(i, :);
end
ax.FontSize=18;
ylabel('Signal (cps)')
legend([],'C_xH_yO','C_xH_yO_2','C_xH_yO_3','C_xH_yO_4','C_xH_y','C_xH_yN_z(O_p)','C_xH_yS','Remaining','Orientation','horizontal')
t1 = datetime(2023, 8, 2, 00, 00, 00); 
t2 = datetime(2023, 8, 3, 00, 00, 00);
xlim([t1, t2])
pbaspect([4 1 1])
nexttile(2)
scatter(amb_1Hz.time, amb_1Hz.pinene,[],[0.39,0.87,0.75],'.')
hold
plot(amb.time, amb.pinene,'black')
t1 = datetime(2023, 8, 2, 00, 00, 00); 
t2 = datetime(2023, 8, 3, 00, 00, 00);
xlim([t1 t2])
ylim([0 405])
ylabel('\SigmaMonoterpenes (ppt)')
ax=gca;
ax.FontSize=18;
legend('1 Sec','5 Min')
pbaspect([4 1 1])
box on
nexttile(3)
scatter(amb_1Hz.time, aromatics_1hz,[],[0.61,0.62,0.46],'.')
hold
plot(amb.time, aromatics_5min,'black')
t1 = datetime(2023, 8, 2, 00, 00, 00); 
t2 = datetime(2023, 8, 3, 00, 00, 00);
xlim([t1 t2])
ylim([0 3800])
ylabel('\SigmaAromatics (ppt)')
ax=gca;
ax.FontSize=18;
legend('1 Sec','5 Min')
xlabel('Time')
pbaspect([4 1 1])
box on
%%
figure
newColors = [
    0.24,0.15,0.66; 
    0.28,0.28,0.92;   
    0.24,0.44,1.00;
    0.15,0.59,0.92;
    0.03,0.71,0.82;
    0.19,0.78,0.62;
    0.51,0.80,0.35;
    0.66,0.91,0.20;
    0.71,0.99,0.19;
    0.92,1.00,0.47
    ];  
names = ["","\alpha Pinene","","Sabinene","\beta Pinene","","Limonene","","",""];
p = piechart(mt_array,names,Direction="counterclockwise");
p.LabelStyle="name";
p.FaceAlpha = 1;
colororder(newColors)
ax=gca;
ax.FontSize=150;
%%
figure
newColors = [
    0.50,0.35,0.00; 
    0.33,0.23,0.00;   
    0.15,0.10,0.00;
    ]; 
names = ["Benzene","o Xylene","1,2,4-TMB"];
p=piechart(arm_array,names,Direction="counterclockwise");
p.LabelStyle="name";
p.FaceAlpha = 1;
colororder(newColors)
ax=gca;
ax.FontSize=150;

%% SI
clear all
load("F:\Chicago\Publications\AGES Chicago Paper\fig_SI.mat") 
twf11=datetime('2023-7-16 00:00:00');
twf12=datetime('2023-7-17 00:00:00');
%%

figure
tiledlayout(3,1)
nexttile(1)
b=bar(uncal_groups_hourly.time, uncal_groups_hourly_array,'stacked','BarWidth', 1)
ax=gca;
ylim([0 inf])
newColors = [
    0.04, 0.42, 0.21;   
    0.21, 0.57, 0.28;
    0.36, 0.75, 0.35;
    0.68, 0.97, 0.72;
    0.73, 0.51, 0.25;
    0.64, 0.16, 0.08;
    0.82, 0.82, 0.39;
   0.87, 0.87, 0.87;
    ];  
for i = 1:length(b)
    b(i).FaceColor = newColors(i, :);
end
ax.FontSize=18;
ylabel('Signal (cps)')
pbaspect([4 1 1])
tstart=ambient_blsub_uncal.time(1);
tend=ambient_blsub_uncal.time(end);
xlim([tstart, tend])
legend([],'C_xH_yO','C_xH_yO_2','C_xH_yO_3','C_xH_yO_4','C_xH_y','C_xH_yN_z(O_p)','C_xH_yS','Other Ions','Orientation','horizontal')

nexttile(2)
b=bar(uncal_groups_hourly.time,uncal_groups_hourly_frac,'stacked', 'BarWidth', 1) 
for i = 1:length(b)
    b(i).FaceColor = newColors(i, :);
end
ylim([0 1])
ax=gca;
ax.FontSize=18;
ylabel('Species Fraction')
pbaspect([4 1 1])
xlim([tstart, tend])

nexttile(3)
scatter(ambient_blsub_uncal.time, ambient_blsub_uncal.C5H5NHplus,[],[0.06 0.12 0.45],'.')
hold
plot(ambient_5.time, ambient_5.C5H5NHplus,'color',[0.51 0.74 0.84])
ylim([0 inf])
tstart=ambient_blsub_uncal.time(1);
tend=ambient_blsub_uncal.time(end);
xlim([tstart, tend])
ax=gca;
ax.FontSize=18;
ylabel('C_5H_6N^+ Signal (cps)')
y=max(ambient_blsub_uncal.C5H5NHplus);
x_coords = [twf11, twf12, twf12, twf11];
y_coords = [0, 0, y, y];
p = patch(x_coords, y_coords, 'black'); 
set(p, 'FaceAlpha', 0.15); 
set(p, 'EdgeColor', 'none'); 
x_coords = [twf21, twf22, twf22, twf21];
p = patch(x_coords, y_coords, 'black'); 
set(p, 'FaceAlpha', 0.15); 
set(p, 'EdgeColor', 'none'); 
legend('1 Second','5 Minute','Wildfires')
pbaspect([4 1 1])