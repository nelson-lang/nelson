%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--NO USER MODULES-->
%=============================================================================
if strcmp(getnelsonmode(), 'GUI')
  banner_path = which('banner');
  asserts.istrue(contains(banner_path, 'libnlsGui_builtin'));
end
%=============================================================================
