%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function st = nmm_list()
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  if isfile(modules_json_path)
    txt = fileread(modules_json_path);
    st = jsondecode(txt);
    if isempty(st) && isdouble(st)
      st = struct([]);
    end
  else
    st = struct([]);
  end
end
%=============================================================================
