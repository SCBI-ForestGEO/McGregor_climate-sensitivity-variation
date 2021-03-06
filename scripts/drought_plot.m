%script to plot drought conditions
%by Kristina Anderson-Teixeira
%~~~~~~~~
clf; clear; close all;

cd '/Users/kteixeira/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/McGregor_climate-sensitivity-variation/data/climate'
NOAA_table = readtable('Formated_NOAA_PDSI_Northern_Virginia_1895_2017.csv');
NOAA_table2 = readmatrix('Formated_NOAA_PDSI_Northern_Virginia_1895_2017.csv');
year=NOAA_table2(:,1);
month=NOAA_table2(:,2);
date=datenum([year month 15+0*month]);
PDSI=NOAA_table2(:,5);

dr=[1966 1977 1999];
colors= [ 1 .4 .7; .2 .7 .2 ; .4 .4 1];
figure(1)
for d=1:3
    fyr=dr(d);
    index=find(year>(fyr-5) & year<(fyr+5));
    date_rel=date(index)-datenum([fyr 1 1]).* (1+0*(index));
    plot(date_rel,PDSI(index), 'LineWidth',2, 'Color', colors(d,:)); hold on;
end

xlim([-365*2+1 365*3+1]);
set(gca, 'XTick', (-365.25*2+2 : 365.25/3 : 365.25*3+2 ));
datetick('x','mmm','keepticks')

xline(121,'-r',{'peak growing season','of drought year'},'LineWidth',4);
xline(243,'-r','LineWidth',4);
xline(-365+1,'--');
xline(0+1,'--');
xline(365+1,'--');
xline(365*2+1,'--');



xlabel('month')
ylabel('PDSI')
legend({'1966' '1977' '1999'},'Location','Southeast')
legend('Boxoff')

set(gcf, 'Position',[200 200 1000 400])

shg

cd '/Users/kteixeira/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/McGregor_climate-sensitivity-variation/manuscript/tables_figures/publication'
print('figureS1_drought_plot', '-djpeg')

