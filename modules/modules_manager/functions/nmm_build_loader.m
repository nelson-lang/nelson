%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_build_loader(MODULE_NAME, MODULE_PATH)
  filewrite([MODULE_PATH, '/loader.m'], content(MODULE_NAME));
end
%=============================================================================
function txt = content(MODULE_NAME)
  txt = ["%=============================================================================";
  "% Copyright (c) 2018-present Allan CORNET (Nelson)";
  "%";
  "% This file is released under the 3-clause BSD license. See COPYING-BSD.";
  "%=============================================================================";
  "% generated file by nmm_build_loader";
  "%=============================================================================";
  "addmodule(fileparts(nfilename('fullpath')), '" + MODULE_NAME + "');";
  "%============================================================================="];
end
