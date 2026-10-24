%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--WEB GUI REQUIRED--> run with: nelson-webview --file runtests_web.m
%=============================================================================
% Headless figure host: uigridlayout has no Qt widget, but performLayout must
% still lay children out, against the figure render size, not a widget.
f = figure();
g = uigridlayout(f, [1 2]);
b1 = uibutton(g, 'Text', 'left');
b2 = uibutton(g, 'Text', 'right');
p1 = get(b1, 'Position');
p2 = get(b2, 'Position');
% two side-by-side cells: same y/height, different x, plausible widths
assert_isequal(p1(2), p2(2));
assert_isequal(p1(4), p2(4));
assert_istrue(p2(1) > p1(1) + 10);
assert_istrue(p1(3) > 50);
% cells span a figure-sized grid (default web figure is a few hundred px wide)
assert_istrue(p2(1) + p2(3) > 200);
close(f);
%=============================================================================
