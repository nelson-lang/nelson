%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Logical-valued switch state used by properties that accept 'on' and 'off'.
% Deriving from logical is what makes `if h.Visible`, `h.Visible == true` and
% logical(h.Visible) behave like the plain flag they represent, while the
% enumeration keeps 'on'/'off' as the displayed and char() form.
%=============================================================================
classdef OnOffSwitchState < logical
  enumeration
    off(false)
    on(true)
  end

  methods
    function disp(obj)
      if numel(obj) == 1
        fprintf('\n  %s enumeration\n\n    %s\n\n', class(obj), char(obj));
      else
        for k = 1:numel(obj)
          fprintf('    %s', char(obj(k)));
        end
        fprintf('\n\n');
      end
    end

    function display(obj, varargin)
      if nargin > 1 && ischar(varargin{1})
        fprintf('\n%s =\n', varargin{1});
      end
      disp(obj);
    end
  end
end
%=============================================================================
