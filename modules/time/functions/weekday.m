%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = weekday(varargin)
  narginchk(1, 3);
  nargoutchk(0, 2);
  
  D = varargin{1};
  format = 'short';
  language = 'en_US';
  
  if (nargin == 2)
    option = varargin{2};
    supportedFormat= strcmpi(option, 'long') ||  strcmpi(option, 'short') 
    supportedLanguage = strcmpi(option, 'local') || strcmpi(option, 'en_US');
    if ~supportedFormat &&  ~supportedLanguage
      error('Nelson:weekday:InvalidOption', _('"long" or "short" option expected.'));
    end
    if supportedFormat
      format = option;
    end
    if supportedLanguage
      language = option;
    end
  end
  
  if (nargin == 3)
    format = varargin{2};
    supportedFormat= strcmpi(format, 'long') ||  strcmpi(format, 'short') 
    if ~supportedFormat
      error('Nelson:weekday:InvalidOption', _('"long" or "short" option expected.'));
    end
    
    language = varargin{3};
    supportedLanguage = strcmpi(language, 'local') || strcmpi(language, 'en_US');
    if ~supportedLanguage
      error('Nelson:weekday:InvalidOption', _('"local" or "en_US" option expected.'));
    end
  end
  
  if (isstring(D) || iscellstr(D) || isnumeric(D))
    endsize = size(D);
  elseif (ischar(D))
    sz = size(D);
    endsize = [sz(1), 1];
  end
  if (isstring(D) || ischar(D) || iscellstr (D))
    D = datenum (D);
  end
  D = floor (reshape (mod (D - 733048, 7), endsize));
  D(~D) = 7;
  varargout{1} = D;
  if (nargout == 2)
    if (strcmpi (format, 'long'))
      if strcmpi(language, 'local')
        names = {_("Sunday"), _("Monday"), _("Tuesday"), _("Wednesday"), _("Thursday"), _("Friday"), _("Saturday")};
        names = string(names);
      else
        names = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
      end
    else
      if strcmpi(language, 'local')
        names = {_("Sun"), _("Mon"), _("Tue"), _("Wed"), _("Thu"), _("Fri"), _("Sat")};
        names = string(names);
      else
        names = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
      end
    end
    
    varargout{2} = char(names(D));
  end
end