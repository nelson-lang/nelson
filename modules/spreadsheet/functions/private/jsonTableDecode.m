%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [root, rootIsArray, columns] = jsonTableDecode(filename, flags, location)
  % Reads a JSON file and decodes it with jsondecode.
  % flags: AllowComments, AllowInfAndNaN, AllowTrailingCommas (lenient extensions).
  % With a table 'location' (see jsonTableColumnar), a table node made of
  % flat objects is decoded straight into columns (Names, Columns and the
  % Selector of the node, see __jsondecode_columns__) and root is not
  % built; otherwise 'columns' is empty.
  if ~isfile(filename)
    error('Nelson:spreadsheet:fileNotFound', 'File not found: %s', filename);
  end
  columns = [];
  if ~isempty(location)
    [ok, names, values, selector] = __jsondecode_columns__(filename, location{:});
    if ok
      % strict JSON: the lenient extensions below have nothing to change
      root = [];
      rootIsArray = false;
      columns = struct('Names', {names}, 'Columns', {values}, 'Selector', selector);
      return
    end
  end
  txt = fileread(filename, 'char', 'native', 'UTF-8');
  stringPattern = '"(?:[^"\x5c]|\x5c.)*"';
  txt = handleComments(txt, stringPattern, flags.AllowComments);
  txt = handleTrailingCommas(txt, stringPattern, flags.AllowTrailingCommas);
  txt = handleInfAndNaN(txt, stringPattern, flags.AllowInfAndNaN);
  first = regexp(txt, '\S', 'match', 'once');
  rootIsArray = strcmp(first, '[');
  root = jsondecode(txt);
end
%=============================================================================
function txt = removeOutsideStrings(txt, stringPattern, removedPattern)
  [parts, found] = regexp(txt, [stringPattern, '|', removedPattern], 'split', 'match');
  isRemoved = cellfun(@(x) x(1) ~= '"', found);
  if ~any(isRemoved)
    return
  end
  found(isRemoved) = {''};
  parts = [parts; [found, {''}]];
  txt = [parts{:}];
end
%=============================================================================
function txt = handleComments(txt, stringPattern, allowed)
  if isempty(strfind(txt, '//')) && isempty(strfind(txt, '/*'))
    return
  end
  stripped = removeOutsideStrings(txt, stringPattern, '//[^\n]*|/\*.*?\*/');
  if isequal(stripped, txt)
    return
  end
  if ~allowed
    error('Nelson:spreadsheet:jsonCommentsNotAllowed', 'Comments are not allowed in the JSON file (AllowComments is false).');
  end
  txt = stripped;
end
%=============================================================================
function txt = handleTrailingCommas(txt, stringPattern, allowed)
  if isempty(regexp(txt, ',\s*[\]}]', 'once'))
    return
  end
  fixed = removeOutsideStrings(txt, stringPattern, ',(?=\s*[\]}])');
  if isequal(fixed, txt)
    return
  end
  if ~allowed
    error('Nelson:spreadsheet:jsonTrailingCommasNotAllowed', 'Trailing commas are not allowed in the JSON file (AllowTrailingCommas is false).');
  end
  txt = fixed;
end
%=============================================================================
function txt = handleInfAndNaN(txt, stringPattern, allowed)
  if isempty(regexp(txt, '(NaN|Inf)', 'once'))
    return
  end
  [outside, inside] = regexp(txt, stringPattern, 'split', 'match');
  found = ~cellfun('isempty', regexp(outside, '[\[:,]\s*[-+]?(NaN|Inf)', 'once'));
  if ~any(found)
    return
  end
  if ~allowed
    error('Nelson:spreadsheet:jsonInfAndNaNNotAllowed', 'Inf and NaN values are not allowed in the JSON file (AllowInfAndNaN is false).');
  end
  if isempty(strfind(txt, 'Infinity'))
    return
  end
  outside = regexprep(outside, 'Infinity', 'Inf');
  parts = [outside; [inside, {''}]];
  txt = [parts{:}];
end
%=============================================================================
