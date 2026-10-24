%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
figure();
T = text(0.5, 0.5, 'short label', 'FontSize', 10);
drawnow();
bb1 = T.Extent;
T.Color = [1 0 0];
drawnow();
asserts.isequal(T.Extent, bb1);
T.String = 'a much longer label';
drawnow();
bb2 = T.Extent;
asserts.istrue(bb2(3) > bb1(3));
T.FontSize = 20;
drawnow();
bb3 = T.Extent;
asserts.istrue(bb3(4) >= bb2(4));
%=============================================================================
figure();
T = text([0 1], [0 1], {'first', 'second'});
asserts.isequal(T(1).String, 'first');
asserts.isequal(T(2).String, 'second');
%=============================================================================
T = text([0 1], [1 0], {{'line 1', 'line 2'}, 'single'});
asserts.isequal(T(1).String, ['line 1', char(10), 'line 2']);
asserts.isequal(T(2).String, 'single');
%=============================================================================
T = text(0.5, 0.25, {'top', 'bottom'});
asserts.isequal(T.String, ['top', char(10), 'bottom']);
asserts.isequal(T.Margin, 3);
drawnow();
extent1 = T.Extent;
asserts.istrue(extent1(3) > 0);
asserts.istrue(extent1(4) > 0);
T.String = 'a much wider text label';
drawnow();
extent2 = T.Extent;
asserts.istrue(extent2(3) > extent1(3));
%=============================================================================
T = text(0.25, 0.25, {42});
asserts.isequal(T.String, '42');
close all;
%=============================================================================
