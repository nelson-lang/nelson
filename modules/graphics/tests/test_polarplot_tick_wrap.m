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
f = figure('Visible', 'off');
polarplot(0:0.1:1, 0:0.1:1);
thetaticks([0 90 180 270 360]);
thetaLabels = thetaticklabels();
asserts.isequal(thetaticks(), [0 90 180 270 360]);
asserts.isequal(thetaLabels{end}, ['270', char(176)]);
asserts.isfalse(any(strcmp(thetaLabels, ['360', char(176)])));
thetalim([0 180]);
thetaticks('auto');
asserts.isequal(thetaticks(), 0:30:180);
close(f);
%=============================================================================
