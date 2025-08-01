%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(varargin)
  obj = varargin{1};
  s = varargin{2};
  switch s(1).type
    case '.'
      if length(s) == 1
        name = s(1).subs;
        if ~isprop(obj, name)
          msg = _('No property of the class ''%s'' matches the identifier ''%s''.');
          error(sprintf(msg, 'ss', name));
        end
        st = struct(obj);
        varargout{1} = st.(name);
      elseif length(s) == 2
        % obj.name(), {}, .
        if strcmp(s(2).type, '()')
          st = struct(obj);
          name = s(1).subs;
          indices = s(2).subs;
          varargout{1} = obj.(name)(indices{:});
        else
          error(_('Not a valid indexing expression'));
        end
      else
        error(_('Not a valid indexing expression'));
      end
    case '{}'
      error(_('Illegal indexing structure argument: type ''.'' expected.'));
    otherwise
      error(_('Not a valid indexing expression'));
    end
  end
  %=============================================================================
  