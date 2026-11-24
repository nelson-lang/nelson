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
% Regression: RenderWeb::tri used to emit triangle OUTLINES instead of fills,
% so the 3D axes background (a triangle fan over its hull) showed white rays
% from the apex corner and its walls were never filled. With the fix, the
% wall region of a surf figure rasterizes as filled white (255),
% above the grey figure background (~245).
f = figure();
surf(peaks);
F = getframe(gcf);
sz = size(F.cdata);
% sample a patch well inside the upper-right wall region (fractions of the
% frame so any figure size works; measured: wall ~253, figure grey 245)
r0 = max(1, round(sz(1) * 0.25));
r1 = round(sz(1) * 0.30);
c0 = round(sz(2) * 0.55);
c1 = round(sz(2) * 0.65);
patch = double(F.cdata(r0:r1, c0:c1, :));
% mostly white fill (grid lines allowed): mean well above the figure grey
assert_istrue(mean(patch(:)) > 249);
%=============================================================================
% the figure margin (outside the axes box) stays the figure background grey
m = double(F.cdata(1:5, 1:5, :));
assert_istrue(mean(m(:)) < 249);
close(f);
%=============================================================================
