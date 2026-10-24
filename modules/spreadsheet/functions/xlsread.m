%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [num, txt, raw] = xlsread(varargin)
  narginchk(1, 4);
  nargoutchk(0, 3);

  filename = varargin{1};
  filename = assertXlsxFilename(filename);
  args = [{'FileType', 'spreadsheet'}, parseReadArguments(varargin(2:end))];
  raw = readcell(filename, args{:});
  [num, txt] = splitRawCell(raw);
end
%=============================================================================
function filename = assertXlsxFilename(filename)
  if ~(ischar(filename) || isStringScalar(filename))
    error('Nelson:spreadsheet:invalidFilename', 'Valid filename expected.');
  end
  filename = convertStringsToChars(filename);
  [~, ~, ext] = fileparts(filename);
  if ~strcmpi(ext, '.xlsx')
    error('Nelson:spreadsheet:onlyXlsxSupported', 'Only .xlsx spreadsheet files are supported by this backend.');
  end
end
%=============================================================================
function args = parseReadArguments(argsIn)
  args = {};
  if isempty(argsIn)
    return;
  end
  argsIn = removeBasicMode(argsIn);
  if isempty(argsIn)
    return;
  end
  if numel(argsIn) == 1
    value = argsIn{1};
    if isRangeText(value)
      args = {'Range', value};
    else
      args = {'Sheet', value};
    end
  elseif numel(argsIn) >= 2
    args = {'Sheet', argsIn{1}, 'Range', argsIn{2}};
  end
end
%=============================================================================
function args = removeBasicMode(args)
  if ~isempty(args) && isTextScalar(args{end}) && strcmpi(convertStringsToChars(args{end}), 'basic')
    args(end) = [];
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function tf = isRangeText(value)
  tf = false;
  if ~isTextScalar(value)
    return;
  end
  value = convertStringsToChars(value);
  tf = ~isempty(strfind(value, ':')) || isCellReference(value);
end
%=============================================================================
function tf = isCellReference(value)
  tf = ~isempty(regexp(value, '^\$?[A-Za-z]{1,3}\$?[0-9]+$', 'once'));
end
%=============================================================================
function [num, txt] = splitRawCell(raw)
  dims = size(raw);
  num = NaN(dims);
  txt = cell(dims);
  for k = 1:numel(raw)
    value = raw{k};
    if isnumeric(value) || islogical(value)
      if isscalar(value)
        num(k) = double(value);
        txt{k} = '';
      else
        txt{k} = '';
      end
    elseif ischar(value)
      txt{k} = value;
    elseif isStringScalar(value)
      txt{k} = convertStringsToChars(value);
    else
      txt{k} = '';
    end
  end
end
%=============================================================================
