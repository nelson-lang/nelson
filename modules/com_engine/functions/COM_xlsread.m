%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [numeric_data, text_data, raw_data] = COM_xlsread(varargin)
  
  numeric_data = [];
  text_data = {};
  raw_data = {};
  nRhs = nargin;
  
  sheet = 1;
  range = '';
  
  if nRhs < 1 || nRhs > 3
    error(_('Wrong number of input arguments.'));
  end
  
  if nRhs == 3
    filename = varargin{1};
    sheet = varargin{2};
    range = varargin{3};
    is_range = strfind(range, ':');
    if ~isscalar(is_range)
      error(_('A valid range expected.'));
    end
  end
  
  if nRhs == 2
    filename = varargin{1};
    sheet_or_range = varargin{2};
    if isnumeric(sheet_or_range)
      sheet = double(sheet_or_range);
      if ~isscalar(sheet)
        error(_('A scalar integer value expected.'));
      end
    else
      is_range = strfind(sheet_or_range, ':');
      if isscalar(is_range)
        range = sheet_or_range;
      else
        sheet = sheet_or_range;
      end
    end
  end
  
  if nRhs == 1
    filename = varargin{1};
  end
  
  if ~ischar(filename)
    error(_('Valid filename expected.'));
  else
    [path, name, ext] = fileparts(filename);
    if ~isempty(strfind(path, '..'))
      error(_('An absolute path expected.'));
    end
    idx_sep = strfind(path, '/');
    if ~isempty(idx_sep)
      path(idx_sep) = '\';
    end
    filename = [path, '\', name, ext];
    if ~isfile(filename)
      error(_('A valid filename expected.'));
    end
  end
  
  try
    excelApplication = actxserver('Excel.Application');
  catch
    error(_('Excel application expected.'));
  end
  
  excelApplication.DisplayAlerts = false;
  workbooks = excelApplication.Workbooks;
  r = invoke(workbooks, 'Open', filename);
  workSheets = excelApplication.ActiveWorkbook.Sheets;
  nbSheets = double(workSheets.Count);
  
  if isnumeric(sheet)
    wsheet = get(workSheets,'item', sheet);
  end
  
  if ischar(sheet)
    wsheet = [];
    for idx = 1:nbSheets
      s = get(workSheets, 'item', idx);
      if strcmp(s.Name, sheet) == true
        wsheet = s;
      end
    end
    if isempty(wsheet)
      excelApplication.Quit;
      delete(excelApplication);
      error(_('sheet name not found.'));
    end
  end
  
  if isempty(range)
    eRange =  wsheet.UsedRange;
  else
    eRange = get(excelApplication.Activesheet,'Range', range);
  end
  data = eRange.value;
  
  excelApplication.Quit;
  delete(excelApplication);
  clear excelApplication
  
  if isempty(data)
    raw_data = [];
    text_data = {};
    numeric_data = [];
    return
  end
  
  text_data = cell(size(data));
  text_data(:) = {''};
  is_char = cellfun('isclass', data, 'char');
  
  any_text = any(is_char(:));
  if ~any_text
    text_data = {};
  else
    text_data(is_char) = data(is_char);
    data(is_char)={NaN};
  end
  clear is_char
  
  is_empty = cellfun('isempty', data);
  data(is_empty) = {NaN};
  
  numeric_data = cellfun('double', data,'UniformOutput', true);
  clear temp_cell
  is_nan = isnan(numeric_data);
  if all(is_nan(:))
    numeric_data = [];
    raw_data = data
    return;
  end
  
  if all(cellfun('islogical', data))
    numeric_data = logical(numeric_data);
  end
  
  if isempty(numeric_data)
    numeric_data = [];
  end
  raw_data = data;
end
%=============================================================================
