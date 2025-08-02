%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure();
s = scatter(1, 1);
assert_isequal(s.Type, 'scatter');
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor , 'flat');
assert_isequal(s.MarkerFaceColor , 'none');
assert_isequal(s.LineWidth , 0.5000);
assert_isequal(s.XData , 1);
assert_isequal(s.YData , 1);
assert_isequal(s.SizeData , 36);
assert_isequal(s.CData , [0 0.4470 0.7410]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = randn (100, 1);
y = randn (100, 1);
scatter (x, y)
%=============================================================================
f = figure();
theta = linspace(0,1,500);
x = exp(theta).*sin(100*theta);
y = exp(theta).*cos(100*theta);
s = scatter(x,y);
%=============================================================================
f = figure();
theta = linspace(0,1,500);
x = exp(theta).*sin(100*theta);
y = exp(theta).*cos(100*theta);
s = scatter(x, y ,'filled');
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor , 'none');
assert_isequal(s.MarkerFaceColor , 'flat');
%=============================================================================
rng('default') % For reproducibility
figure();
theta = linspace(0,2*pi,150);
x = sin(theta) + 0.75*rand(1,150);
y = cos(theta) + 0.75*rand(1,150);  
sz = 140;
s = scatter(x,y,sz,'d');
assert_isequal(s.Marker, 'd');
assert_isequal(s.MarkerEdgeColor , 'flat');
assert_isequal(s.MarkerFaceColor , 'none');
assert_isequal(s.SizeData, 140)
%=============================================================================
rng('default') % For reproducibility
figure();
x = randn (100, 1);
y = randn (100, 1);
s = scatter (x, y, 50);
assert_isequal(s.SizeData, 50);
%=============================================================================
rng('default') % For reproducibility
figure();
x = linspace(0,3*pi,200);
y = cos(x) + rand(1,200);
sz = 1:200;
c = 1:length(x);
s = scatter(x,y,sz,c,'filled');
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'none');
assert_isequal(s.MarkerFaceColor, 'flat');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.SizeData, sz);
assert_isequal(s.CData, c);
%=============================================================================
rng('default') % For reproducibility
figure();
x = linspace(0,3*pi,200);
y = cos(x) + rand(1,200);
c = linspace(1,10,length(x));
s = scatter(x,y,[],c);
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, c);
%=============================================================================
rng('default') % For reproducibility
figure();
theta = linspace(0,2*pi,300);
x = sin(theta) + 0.75*rand(1,300);
y = cos(theta) + 0.75*rand(1,300);  
sz = 40;
s = scatter(x,y,sz,'MarkerEdgeColor',[0 .5 .5], 'MarkerFaceColor',[0 .7 .7],  'LineWidth', 1.5);
assert_isequal(s.MarkerEdgeColor, [0 0.5000 0.5000]);
assert_isequal(s.MarkerFaceColor, [0 0.7000 0.7000]);
assert_isequal(s.LineWidth, 1.5000);
assert_isequal(s.SizeData, 40);
assert_isequal(s.CData, [0 0.4470 0.7410]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = linspace(0,3*pi,200);
y = cos(x) + rand(1,200);
% Top plot
ax1 = subplot(2,1, 1);
s1 = scatter(ax1,x,y);
assert_isequal(s1.Marker, 'o');
assert_isequal(s1.MarkerEdgeColor, 'flat');
assert_isequal(s1.MarkerFaceColor, 'none');
assert_isequal(s1.LineWidth, 0.5000);
assert_isequal(s1.XData, x);
assert_isequal(s1.YData, y);
assert_isequal(s1.SizeData, 36);
assert_isequal(s1.CData, [0 0.4470 0.7410]);
% Bottom plot
ax2 = subplot(2,1, 2);
s2 = scatter(ax2,x,y,'filled','d');
assert_isequal(s2.Marker, 'd');
assert_isequal(s2.MarkerEdgeColor, 'none');
assert_isequal(s2.MarkerFaceColor, 'flat');
assert_isequal(s2.LineWidth, 0.5000);
assert_isequal(s2.XData, x);
assert_isequal(s2.YData, y);
assert_isequal(s2.SizeData, 36);
assert_isequal(s2.CData, [0 0.4470 0.7410]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = rand(50,5);
y = randn(50,5) + (5:5:25);
s = scatter(x,y, 'filled');
assert_isequal(size(s), [1 5]);
assert_isequal(s(1).CData, [0    0.4470    0.7410]);
assert_isequal(s(2).CData,[0.8500    0.3250    0.0980]);
assert_isequal(s(3).CData,[0.9290    0.6940    0.1250]);
assert_isequal(s(4).CData,[0.4940    0.1840    0.5560]);
assert_isequal(s(5).CData,[0.4660    0.6740    0.1880]);
%=============================================================================
f = figure();
c = 1:4;
s = scatter(1:4,[2 5 3 7],[],c);
colormap(gca,'winter')
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.XData, [1 2 3 4]);
assert_isequal(s.YData, [2 5 3 7]);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, [1 2 3 4]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = randn (100, 1);
y = randn (100, 1);
c = sqrt (x.^2 + y.^2);
s = scatter (x, y, [], c);
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isapprox(s.XData, x(:)', 1e-5);
assert_isapprox(s.YData, y(:)', 1e-5);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, c);
%=============================================================================
f = figure();
x = [0.777753, 0.093848, 0.183162, 0.399499, 0.337997, 0.686724, 0.073906, 0.651808, 0.869273, 0.137949];
y = [0.37460, 0.25027, 0.19510, 0.51182, 0.54704, 0.56087, 0.24853, 0.75443, 0.42712, 0.44273];
s = scatter (x, y, [], 'r', 's');
assert_isequal(s.Marker, 's');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, [1 0 0]);
%=============================================================================
f = figure();
x = [0.777753, 0.093848, 0.183162, 0.399499, 0.337997, 0.686724, 0.073906, 0.651808, 0.869273, 0.137949];
y = [0.37460, 0.25027, 0.19510, 0.51182, 0.54704, 0.56087, 0.24853, 0.75443, 0.42712, 0.44273];
s = scatter (x, y, [], 'green', 's');
assert_isequal(s.Marker, 's');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, [0 1 0]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = rand(100,1);
y = rand(100,1);
MarkerColor = [0.5, 0.5, 0.5];
s = scatter(x, y, 40, MarkerColor, 'filled');
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'none');
assert_isequal(s.MarkerFaceColor, 'flat');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.SizeData, 40);
assert_isequal(s.CData, [0.5000 0.5000 0.5000]);
%=============================================================================
f = figure;
x = 1:5;
s1 = scatter(x,[6 3 9 10 7],100,"filled");
assert_isequal(s1.Marker, 'o');
assert_isequal(s1.MarkerEdgeColor, 'none');
assert_isequal(s1.MarkerFaceColor, 'flat');
assert_isequal(s1.LineWidth, 0.5000);
assert_isequal(s1.XData, [1 2 3 4 5]);
assert_isequal(s1.YData, [6 3 9 10 7]);
assert_isequal(s1.SizeData, 100);
assert_isequal(s1.CData, [0 0.4470 0.7410]);
hold on
s2 = scatter(x,[16 13 19 20 17],100,"filled");
assert_isequal(s2.Marker, 'o');
assert_isequal(s2.MarkerEdgeColor, 'none');
assert_isequal(s2.MarkerFaceColor, 'flat');
assert_isequal(s2.LineWidth, 0.5000);
assert_isequal(s2.XData, [1 2 3 4 5]);
assert_isequal(s2.YData, [16 13 19 20 17]);
assert_isequal(s2.SizeData, 100);
assert_isequal(s2.CData, [0.8500 0.3250 0.0980]);
s3 = scatter(x,[26 23 29 33 27],100,"filled");
assert_isequal(s3.Marker, 'o');
assert_isequal(s3.MarkerEdgeColor, 'none');
assert_isequal(s3.MarkerFaceColor, 'flat');
assert_isequal(s3.LineWidth, 0.5000);
assert_isequal(s3.XData, [1 2 3 4 5]);
assert_isequal(s3.YData, [26 23 29 33 27]);
assert_isequal(s3.SizeData, 100);
assert_isequal(s3.CData, [0.9290 0.6940 0.1250]);
hold off
%=============================================================================
rng('default') % For reproducibility
figure();
theta = linspace(0,2*pi,150);
x = sin(theta) + 0.75*rand(1,150);
y = cos(theta) + 0.75*rand(1,150);  
sz = 140;
s = scatter(x,y,sz,'h');
assert_isequal(s.Marker, 'h');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.XData, x);
assert_isequal(s.YData, y);
assert_isequal(s.SizeData, 140);
assert_isequal(s.CData, [0 0.4470 0.7410]);
%=============================================================================
rng('default') % For reproducibility
figure();
theta = linspace(0,2*pi,150);
x = sin(theta) + 0.75*rand(1,150);
y = cos(theta) + 0.75*rand(1,150);  
sz = 140;
s = scatter(x,y,sz,'p');
assert_isequal(s.Marker, 'p');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.XData, x);
assert_isequal(s.YData, y);
assert_isequal(s.SizeData, 140);
assert_isequal(s.CData, [0 0.4470 0.7410]);
%=============================================================================
rng('default') % For reproducibility
x = randn (100, 1);
y = randn (100, 1);
assert_checkerror('scatter (x, y, 1:0.5:50)', _('Size of S must match X, Y.'))
%=============================================================================
rng('default') % For reproducibility
figure();
x = randn (100, 1);
y = randn (100, 1);
s = scatter (x, y);
s.SizeData = 50;
assert_isequal(s.SizeData, 50);
%=============================================================================
rng('default') % For reproducibility
figure();
x = linspace(0,3*pi,200);
y = cos(x) + rand(1,200);
sz = linspace(1,100,200);
s = scatter(x,y,sz);
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 0.5000);
assert_isequal(s.XData, x);
assert_isequal(s.YData, y);
assert_isequal(s.SizeData, sz);
assert_isequal(s.CData, [0 0.4470 0.7410]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = rand(1,100);
y = rand(1,100);
s = scatter(x,y,75, "MarkerEdgeColor", "b", "MarkerFaceColor", [0 0.7 0.7]);
assert_isequal(s.Marker, 'o')
assert_isequal(s.MarkerEdgeColor, [0 0 1])
assert_isequal(s.MarkerFaceColor, [0 0.7000 0.7000])
assert_isequal(s.LineWidth, 0.5000)
assert_isequal(s.XData, x)
assert_isequal(s.YData, y)
assert_isequal(s.SizeData, 75)
assert_isequal(s.CData, [0 0.4470 0.7410])
%=============================================================================
figure();
s = scatter(1, 1 , 'x', 'r', 'LineWidth', 1.5);
assert_isequal(s.Marker, 'x');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 1.5000);
assert_isequal(s.XData, 1);
assert_isequal(s.YData, 1);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, [1 0 0]);
%=============================================================================
figure();
s = scatter(1, 1, 'o', 'g', 'filled', 'LineWidth', 2);
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'none');
assert_isequal(s.MarkerFaceColor, 'flat');
assert_isequal(s.LineWidth, 2);
assert_isequal(s.XData, 1);
assert_isequal(s.YData, 1);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, [0 1 0]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = rand(1,100);
y = rand(1,100);
ax = gca();
s = scatter(ax, x, y, 100, 'b', 'Marker', 'x', 'LineWidth', 3);
assert_isequal(s.Marker, 'x');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 3);
assert_isequal(s.XData, x);
assert_isequal(s.YData, y);
assert_isequal(s.SizeData, 100);
assert_isequal(s.CData, [0 0 1]);
%=============================================================================
rng('default') % For reproducibility
figure;
y = rand(1,5); 
s = scatter(1:5, y, 100, 'b', 'LineWidth', 2);
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 2);
assert_isequal(s.XData, [1 2 3 4 5]);
assert_isequal(s.YData, y);
assert_isequal(s.SizeData, 100);
assert_isequal(s.CData, [0 0 1]);
%=============================================================================
rng('default') % For reproducibility
f = figure;
ax = gca();
s = scatter(ax, linspace(0,10,10), rand(1,10), 's', 'g', 'LineWidth', 2);
assert_isequal(s.Marker, 's');
assert_isequal(s.MarkerEdgeColor, 'flat');
assert_isequal(s.MarkerFaceColor, 'none');
assert_isequal(s.LineWidth, 2);
assert_isequal(s.SizeData, 36);
assert_isequal(s.CData, [0 1 0]);
%=============================================================================
figure;
X = [1 2 3; 4 5 6];
Y = [2 3 4; 5 6 7];
s = scatter(X, Y, 'x', 'm', 'LineWidth', 2);
assert_isequal(size(s), [1 3]);
assert_isequal(s(1).Marker, 'x');
assert_isequal(s(1).MarkerEdgeColor, 'flat');
assert_isequal(s(1).MarkerFaceColor, 'none');
assert_isequal(s(1).LineWidth, 2);
assert_isequal(s(1).XData, [1 4]);
assert_isequal(s(1).YData, [2 5]);
assert_isequal(s(1).SizeData, 36);
assert_isequal(s(1).CData, [1 0 1]);
assert_isequal(s(2).Marker, 'x');
assert_isequal(s(2).MarkerEdgeColor, 'flat');
assert_isequal(s(2).MarkerFaceColor, 'none');
assert_isequal(s(2).LineWidth, 2);
assert_isequal(s(2).XData, [2 5]);
assert_isequal(s(2).YData, [3 6]);
assert_isequal(s(2).SizeData, 36);
assert_isequal(s(2).CData, [1 0 1]);
assert_isequal(s(3).Marker, 'x');
assert_isequal(s(3).MarkerEdgeColor, 'flat');
assert_isequal(s(3).MarkerFaceColor, 'none');
assert_isequal(s(3).LineWidth, 2);
assert_isequal(s(3).XData, [3 6]);
assert_isequal(s(3).YData, [4 7]);
assert_isequal(s(3).SizeData, 36);
assert_isequal(s(3).CData, [1 0 1]);
%=============================================================================
