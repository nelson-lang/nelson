%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = methods(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  obj = varargin{1};
  if ~isa(obj, 'dictionary')
    msg = _("'dictionary' class expected.");
    error(msg);
  end
  methods_list = {'dictionary', ... 
  'isConfigured', ... 
  'keys', ... 
  'types', ... 
  'isKey', ... 
  'numEntries', ... 
  'insert', ...
  'lookup', ...
  'remove', ...
  'values'};
  
  varargout = {};
  if nargout == 1
    varargout{1} = methods_list';
    return
  end
  fmt = format();
  LineSpacing = fmt.LineSpacing;
  if strcmp(LineSpacing, 'loose')
    fprintf('\n')
  end
  msgfmt = _('Methods for class %s:\n');
  msg = sprintf(msgfmt, class(obj));
  fprintf(msg)
  if strcmp(LineSpacing, 'loose')
    fprintf('\n')
  end
  for e = methods_list
    fprintf('    %s\n', e{1});
  end
  if strcmp(LineSpacing, 'loose')
    fprintf('\n')
  end
end
%=============================================================================
