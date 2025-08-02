%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, compiler] = loadcompilerconf()
  if ~ispc()
    compiler = 'unix';
    status = false;
    return
  end
  compiler = '';
  status = false;
  arch = computer('arch');
  jsonfile = [prefdir(), '/compiler_', arch, '.json'];
  if isfile(jsonfile)
    txt = fileread(jsonfile);
    json = jsondecode(txt);
    for f = fieldnames(json)(:)'
      if strcmp(f{1},'COMPILER_CHOICE')
        compiler = getfield(json,f{1});
        status = true;
      else
        val = getfield(json, f{1});
        if strcmp(f{1},'PATH')
          setenv('PATH', [val, ';', getenv('PATH')]);
        else
          previous = getenv(f{1});
          if strcmp(previous, '')
            setenv(f{1}, val);
          else
            if ~contains(previous, val)
              setenv(f{1}, [val, ';', previous]);
            end
          end
        end
      end
    end
  end
end
%=============================================================================
