%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = table2struct(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  if nargin == 2
    error(_('Invalid number of input arguments.'));
  end
  toScalar = false;
  if nargin == 3
    second_arg = varargin{2};
    second_arg = convertStringsToChars(varargin{2});
    if (~ischar(second_arg))
      error(_('Invalid second argument.'));
    end
    if (~strcmpi(second_arg, 'toscalar'))
      error(_('Invalid second argument.'));
    end
    third_arg = varargin{3};
    if isscalar(third_arg) && (isdouble(third_arg) || islogical(third_arg)) 
      toScalar = third_arg == 1;
    else
      error(_('Invalid third argument.'));
    end
  end
  st = struct();
  T = varargin{1};
  if (~istable(T))
    error(_('Invalid first argument.'));
  end
  asStruct = struct(T);
  if (toScalar)
    for j = 1:width(T)
      st.(asStruct.Properties.VariableNames{j}) = asStruct.data.(asStruct.Properties.VariableNames{j});
    end
  else
    nRows = height(T);
    st = repmat(struct(), nRows, 1); 
    names = fieldnames(asStruct.data);
    for i = 1:nRows
      for j = 1:width(T)
        st(i).(names{j}) = T{i, j};
      end
    end        
  end
  varargout{1} = st;
end
%=============================================================================
