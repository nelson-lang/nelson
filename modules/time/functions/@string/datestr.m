%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = datestr(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  dateStringIn = varargin{1};
  epochs = [];
  
  if nargin == 1
    % DateString = datestr(DateStringIn)
    epochs = datenum(dateStringIn);
    varargout{1} = datestr(epochs);
    return
  end
  if nargin == 2
    mustBeTextScalar(varargin{2}, 2);
    formatOutOrLocal = convertStringsToChars(varargin{2});
    if (~ischar(formatOutOrLocal))
      error(_('Second argument must be a scalar string or row vector of characters.'));
    end
    if strcmp(formatOutOrLocal, 'local')
      % DateString = datestr(DateStringIn, 'local')
      epochs = datenum(dateStringIn);
      varargout{1} = datestr(epochs, 'local');
    else
      % DateString = datestr(DateStringIn, formatOut)
      try
        epochs = datenum(dateStringIn);
      catch
        epochs = datenum(dateStringIn, formatOutOrLocal);
      end
      varargout{1} = datestr(epochs, formatOutOrLocal);
    end
    return
  end
  if nargin == 3
    formatOut = convertStringsToChars(varargin{2});
    formatOutOrLocal = convertStringsToChars(varargin{3});
    if ischar(formatOutOrLocal)
      if ~strcmp(formatOutOrLocal, 'local')
        error(_("Third argument must be a 'local'."));
      end
      % DateString = datestr(DateStringIn, formatOut, 'local')
      epochs = datenum(dateStringIn, formatOut);
      varargout{1} = datestr(epochs, formatOut, 'local');
    elseif (isnumeric(formatOutOrLocal) && isscalar(formatOutOrLocal))
      % DateString = datestr(DateStringIn, formatOut, PivotYear)
      epochs = datenum(dateStringIn, formatOutOrLocal);
      varargout{1} = datestr(epochs, formatOut);
    else
      error(_('Third argument must be a numeric scalar.'));
    end
    return
  end
  if nargin == 4
    % DateString = datestr(DateStringIn, formatOut, PivotYear, 'local')
    mustBeTextScalar(varargin{2}, 2);
    formatOut = convertStringsToChars(varargin{2});
    mustBeNumeric(varargin{3}, 3);
    pivotYear = varargin{3};
    if (~isscalar(pivotYear))
      error(_('Third argument must be a numeric scalar.'));
    end
    mustBeTextScalar(varargin{4}, 4);
    local = convertStringsToChars(varargin{4});
    if (~ischar(local))
      error( _("Fourth argument must be a 'local'."));
    end
    if (strcmp(local, 'local') == false)
      error( _("Fourth argument must be a 'local'."));
    end
    epochs = datenum(dateStringIn, formatOut, pivotYear);
    varargout{1} = datestr(epochs, formatOut, 'local');
    return
  end
end  
%=============================================================================
