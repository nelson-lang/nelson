%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = COM_xlswrite(filename, value_to_save, varargin)
  status = false;
  message = '';
  
  nRhs = nargin;
  nLhs = nargout;
  
  if nRhs < 2 || nRhs > 4
    error(_('Wrong number of input arguments.'));
  end
  if nLhs > 2
    error(_('Wrong number of output arguments.'));
  end
  
  sheet = 1;
  range = '';
  
  if nRhs == 3
    sheet_or_range = varargin{1};
    if isnumeric(sheet_or_range)
      sheet = double(sheet_or_range);
    else
      is_range = strfind(sheet_or_range, ':');
      if isscalar(is_range)
        range = sheet_or_range;
      else
        if ischar(sheet_or_range)
          sheet = sheet_or_range;
        else
          status = false;
          message = _('A valid range expected.');
          if nLhs > 0
            varargout{1} = status;
            varargout{2} = message;
            return;
          else
            error(message);
          end
        end
      end
    end
  end
  
  if nRhs == 4
    sheet = varargin{1};
    range = varargin{2};
    is_range = strfind(range, ':');
    if ~isscalar(is_range)
      status = false;
      message = _('A valid range expected.');
      if nLhs > 0
        varargout{1} = status;
        varargout{2} = message;
        return;
      else
        error(message);
      end
    end
  end
  
  if ~ischar(filename)
    status = false;
    message = _('Valid filename expected.');
    if nLhs > 0
      varargout{1} = status;
      varargout{2} = message;
      return;
    else
      error(message);
    end
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
  end
  if isempty(value_to_save)
    status = false;
    message = _('No empty matrix expected.');
    if nLhs > 0
      varargout{1} = status;
      varargout{2} = message;
      return;
    else
      error(message);
    end
  end
  if ndims(value_to_save) > 2
    status = false;
    message = _('Only 2D matrix supported.');
    if nLhs > 0
      varargout{1} = status;
      varargout{2} = message;
      return;
    else
      error(message);
    end
  end
  
  if isempty(range)
    [m, n] = size(value_to_save);
    range = ['A1', ':', COM_range(m, n)];
  end
  
  if ~COM_range(range)
    error(_('A valid range expected.'));
  end
  
  try
    excelApplication = actxserver('Excel.Application');
  catch
    status = false;
    message = _('Excel application expected.');
    if nLhs > 0
      varargout{1} = status;
      varargout{2} = message;
      return;
    else
      error(message);
    end
  end
  
  try
    excelApplication.DisplayAlerts = false;
    eWorkbook = excelApplication.Workbooks.Add;
    eSheets = excelApplication.ActiveWorkbook.Sheets;
    
    if isnumeric(sheet)
      while double(eSheets.Count) < sheet
        lastsheet = invoke(eSheets, 'Item', eSheets.Count);
        newsheet = invoke(eSheets, 'Add', [], lastsheet);
      end
    else
      lastsheet = invoke(eSheets, 'Item', eSheets.Count);
      newsheet = invoke(eSheets, 'Add', lastsheet);
    end
    if ischar(sheet)
      newsheet.Name = sheet;
    end
    
    eActivesheetRange = get(excelApplication.Activesheet,'Range', range);
    eActivesheetRange.Value = value_to_save;
    
    r = invoke(eWorkbook, 'SaveAs', filename);
    eWorkbook.Saved = 1;
    
  catch
    l = lasterror();
    status = false;
    message = l.message;
    
    excelApplication.Quit;
    delete(excelApplication);
    clear excelApplication
    
    if nLhs > 0
      varargout{1} = status;
      varargout{2} = message;
      return;
    else
      error(message);
    end
  end
  
  excelApplication.Quit;
  delete(excelApplication);
  clear excelApplication
  
  status = true;
  message = '';
  if nLhs > 0
    varargout{1} = status;
    varargout{2} = message;
  end
end
%=============================================================================


