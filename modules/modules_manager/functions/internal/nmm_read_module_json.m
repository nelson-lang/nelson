%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function st = nmm_read_module_json(directory)
  module_json_path = [directory, 'module.json'];
  txt = fileread(module_json_path);
  st = jsondecode(txt);
end
%=============================================================================
