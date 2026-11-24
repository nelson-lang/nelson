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
surf(peaks);
f = gcf();
dl = jsondecode(__web_display_list__(f));
assert_istrue(isfield(dl, 'webglMeshes'));
assert_istrue(length(dl.webglMeshes) >= 1);
assert_istrue(dl.webglMeshes(1).positionsLength > 1000);
%=============================================================================
F = getframe(f);
assert_isequal(class(F.cdata), 'uint8');
sz = size(F.cdata);
assert_isequal(length(sz), 3);
assert_isequal(sz(3), 3);
assert_istrue(sz(1) > 100 && sz(2) > 100);
r = double(F.cdata(:, :, 1));
b = double(F.cdata(:, :, 3));
assert_istrue(max(max(abs(r - b))) > 60);
close(f);
%=============================================================================
