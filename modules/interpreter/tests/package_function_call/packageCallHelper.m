%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [singleLevel, nested, deepest, coexistStatic, coexistPackage] = packageCallHelper()
  singleLevel = Pkg.greet();
  nested = Pkg.util.inner();
  deepest = Pkg.util.more.deepest();
  coexistStatic = Thing.make();
  coexistPackage = Thing.build();
end
%=============================================================================
