%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% logo draws the L-shaped membrane, which relies on eigs (shift-invert) and so
% needs ARPACK. Skip on builds without ARPACK, like test_eigs / test_svds.
try
  probeMembrane = membrane(1, 15); %#ok<NASGU> capture output: nargout>0 avoids the surf() display path
catch ex
  skip_testsuite(strcmp(ex.message, _('ARPACK support is not available.')), ex.message);
  rethrow(ex);
end
%=============================================================================
f = logo();
assert_istrue(isgraphics(f, 'figure'));
% one membrane surface and two lights
s = findobj(f, 'Type', 'surface');
assert_isequal(numel(s), 1);
lights = findobj(f, 'Type', 'light');
assert_isequal(numel(lights), 2);
% the surface is the 201x201 L-shaped membrane (membrane(1,100))
assert_isequal(size(get(s, 'ZData')), [201 201]);
ax = get(s, 'Parent');
assert_isapprox(get(ax, 'CameraViewAngle'), 36.7, 1e-6);
close(f);
%=============================================================================
