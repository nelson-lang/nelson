%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Pull an optional ('Launcher', value) name/value pair out of the argument list
% so the compatibility test_run entry point (which parses positionally) can
% still accept it. Returns the remaining arguments and the launcher value
% ('default' when the pair is absent).
function [args, launcher] = run_extractLauncher(args)
  launcher = 'default';
  k = 1;
  while k <= numel(args)
    if (ischar(args{k}) || isstring(args{k})) && strcmpi(char(args{k}), 'Launcher')
      if k == numel(args)
        error('Nelson:tests_manager:optionValueExpected:optionValueExpectedAfterLauncher', 'Option value expected after ''Launcher''.');
      end
      launcher = char(args{k + 1});
      args(k:k + 1) = [];
      return
    end
    k = k + 1;
  end
end
%=============================================================================
