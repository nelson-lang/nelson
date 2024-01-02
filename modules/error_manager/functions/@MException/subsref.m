%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
        % Implement obj.PropertyName
        st = struct(obj);
        varargout{1} = st.(s.subs);
      elseif length(s) == 2 && strcmp(s(2).type,'()')
        % Implement obj.PropertyName(indices)
        name = s(1).subs;
        indices = s(2).subs;
        st = struct(obj);
        varargout{1} = st.(name)(indices{:});
      else
        [varargout{1:nargout}] = builtin('subsref', obj, s);
      end
    case '()'
      if length(s) == 1
        % Implement obj(indices)
        indices = s.subs;
        st = struct(obj);
        varargout{1} = st(indices{:});
      elseif length(s) == 2 && strcmp(s(2).type,'.')
        % Implement obj(indices).PropertyName
        indices = s(1).subs;
        name = s(2).subs;
        st = struct(obj);
        varargout{1} = st(indices{:}).(name);
      elseif length(s) == 3 && strcmp(s(2).type,'.') && strcmp(s(3).type,'()')
        % Implement obj(indices1).PropertyName(indices2)
        indices1 = s(1).subs;
        name = s(2).subs;
        indices2 = s(3).subs;
        st = struct(obj);
        varargout{1} = st(indices1{:}).(name)(indices2{:});
      else
        % Use built-in for any other expression
        [varargout{1:nargout}] = builtin('subsref', obj, s);
      end
    case '{}'
      error(_('Illegal indexing structure argument: type ''.'' or ''()'' expected.'));
    otherwise
      error(_('Not a valid indexing expression'));
  end
end
%=============================================================================
