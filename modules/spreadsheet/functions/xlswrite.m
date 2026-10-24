%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, message] = xlswrite(varargin)
  narginchk(2, 4);
  nargoutchk(0, 2);

  filename = varargin{1};
  value = varargin{2};
  args = parseWriteArguments(varargin(3:end));
  status = false;
  message = '';
  try
    filename = assertXlsxFilename(filename);
    writeValue(filename, value, args);
    status = true;
  catch ex
    message = ex.message;
  end
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
function args = parseWriteArguments(argsIn)
  args = {'FileType', 'spreadsheet'};
  if isempty(argsIn)
    return;
  end
  if numel(argsIn) == 1
    value = argsIn{1};
    if isRangeText(value)
      args = [args, {'Range', value}];
    else
      args = [args, {'Sheet', value}];
    end
  else
    args = [args, {'Sheet', argsIn{1}, 'Range', argsIn{2}}];
  end
end
%=============================================================================
function writeValue(filename, value, args)
  if istimetable(value)
    writetimetable(value, filename, args{:});
  elseif istable(value)
    writetable(value, filename, args{:});
  elseif iscell(value)
    writecell(value, filename, args{:});
  else
    writematrix(value, filename, args{:});
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
