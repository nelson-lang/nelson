%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [barePkgFn, bareNestedFn, bareDeepFn, bareStatic] = packageBareRefHelper()
  % Bare (no-parenthesis) references to package members, resolved like a call.
  barePkgFn = Pkg.greet;
  bareNestedFn = Pkg.util.inner;
  bareDeepFn = Pkg.util.deep.deephelper;
  bareStatic = Pkg.util.Gadget.spin;
end
%=============================================================================
