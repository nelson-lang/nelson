%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, compiler] = havecompiler()
  persistent havecompilerStatus;
  persistent havecompilerCompiler;
  if isempty(havecompilerStatus)
    if ispc()
      arch = computer('arch');
      jsonfile = [prefdir(), '/compiler_', arch, '.json'];
      status = false;
      compiler = '';
      if isfile(jsonfile)
        txt = fileread(jsonfile);
        json = jsondecode(txt);
        if isfield(json, 'COMPILER_CHOICE')
          status = true;
          compiler = json.COMPILER_CHOICE;
        end
      end
    else
      status = true;
      compiler = 'unix';
    end
    havecompilerStatus = status;
    havecompilerCompiler = compiler;
  else
    status = havecompilerStatus;
    compiler = havecompilerCompiler;
  end
end
%=============================================================================