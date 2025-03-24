%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if (strcmp(getnelsonmode(), 'GUI') == true || ...
  strcmp(getnelsonmode(), 'ADVANCED_ENGINE') == true || ...
  strcmp(getnelsonmode(), 'ADVANCED_TERMINAL') == true)
  addmodule([nelsonroot() '/modules/' 'graphics_io'], 'graphics_io');
end
%=============================================================================
