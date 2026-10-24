%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function st = nmm_list(varargin)
  [args, jsonOutput] = nmm_json_option(varargin);
  if ~isempty(args)
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  if isfile(modules_json_path)
    st = jsondecode(modules_json_path, '-file');
    if isempty(st) && isdouble(st)
      st = struct([]);
    end
  else
    st = struct([]);
  end
  st = nmm_json_output(st, jsonOutput);
end
%=============================================================================
