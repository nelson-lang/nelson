%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function disp(varargin)
  narginchk(1, 1);
  fmt = format();
  LineSpacing = fmt.LineSpacing;
  obj = varargin{1};
  if (~isConfigured (obj))
    fprintf(_('\n  dictionary object not configured.\n\n'));
    return
  end
  if (numEntries(obj) == 0)
    fprintf('\n  dictionary (%s ⟼ %s) with no entries.\n\n', obj.keyType, obj.valueType);
    return
  end
  if (numEntries (obj) == 1)
    entry_str =  'entry';
  else
    entry_str = 'entries';
  end
  if strcmp(LineSpacing, 'loose')
    msgfmt = '\n  dictionary (%s ⟼ %s) with %d %s:\n\n'; 
  else
    msgfmt = '  dictionary (%s ⟼ %s) with %d %s:\n'; 
  end
  fprintf(msgfmt, obj.keyType, obj.valueType, numEntries (obj), entry_str);
  allKeys = obj.allKeys;
  for i = 1:numEntries(obj)
    key = allKeys{i};
    h = dec2hex (keyHash(key));
    val = obj.map.(h);
    key = strtrim(evalc('disp(key)'));
    if strcmp(obj.keyType, 'string')
      key = ['"', key, '"'];
    end
    if strcmp(obj.keyType, 'char')
      key = ['''', key, ''''];
    end
    if isstring(val) && isrow(val)
      nbmax = 42;
      if strlength(val) > nbmax
        aschar = char(val);
        val = [aschar(1:nbmax), '…'];
      end
      valstr = """" + val + """";
    else
        valstr = strtrim(formattedDisplayText(val, 'LineSpacing', LineSpacing));
      end
    if (length(strfind(valstr, char(10))) > 1)
      valstr = getHeaderClassToString(val);
    end
    fprintf("  %s ⟼ %s\n", key, valstr);
  end
  if strcmp(LineSpacing, 'loose')
    fprintf("\n");
  end
end
%=============================================================================
function str = getHeaderClassToString(obj)
  dimensions = size(obj);
  typeName = class(obj);
  str = '';
  for i = 1:numel(dimensions)
    str = [str, sprintf('%d', dimensions(i))];
    if i < numel(dimensions)
        str = [str, '×'];
    end
  end
  str = [str, ' ', typeName];
end
%=============================================================================
