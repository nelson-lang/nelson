%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = gallery(varargin)
  narginchk(1, inf); 
  matname = varargin{1};
  if ~(isstring(matname) || ischar(matname))
    if isnumeric(matname) && (matname == 3 || matname == 5)
      matname = ['gallery' num2str(matname)];
    else
      error(_('Wrong value for #1 argument.'));
    end
  end
  matname = convertStringsToChars(matname);
  
  switch matname
    case 'gallery3'
      classname = class(varargin{1});
      nargout = max(nargout, 1);
      [varargout{1:nargout}] = gallery3(classname);
    case 'gallery5'
      classname = class(varargin{1});
      nargout = max(nargout, 1);
      [varargout{1:nargout}] = gallery5(classname);
    case 'circul'
      if (nargin() ~= 2)
        error(_('circul matrix requires a second argument.'));
      end
      x = varargin{2};
      if nargout() > nargout('circul')
        error(_('Too many output arguments for circul matrix.'));
      end
      [varargout{1:nargout('circul')}] = circul(x);
    case 'grcar'
      if (nargin() < 2 || nargin() > 4)
        error(_('grcar matrix requires between 1 and 3 additional arguments.'));
      end
      n = varargin{2};
      if nargin() >= 3
        upperBand = varargin{3};
      else
        upperBand = [];
      end
      if nargin() == 4
        className = varargin{4};
      else
        className = 'double';
      end
      if nargout() > nargout('grcar')
        error(_('Too many output arguments for grcar matrix.'));
      end
      [varargout{1:nargout('grcar')}] = grcar(n, upperBand, className);
    case 'minij'
      if (nargin() ~= 2)
        error(_('minij matrix requires a second argument.'));
      end
      nSize = varargin{2};
      if nargout() > nargout('minij')
        error(_('Too many output arguments for minij matrix.'));
      end
      [varargout{1:nargout('minij')}] = minij(nSize, 'double');
    case 'dramadah'
      if (nargin() < 2 || nargin() > 4)
        error(_('dramadah matrix requires between 1 and 3 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() >= 3
        patternType = varargin{3};
      else
        patternType = [];
      end
      if nargin() == 4
        className = varargin{4};
      else
        className = 'double';
      end
      if nargout() > nargout('dramadah')
        error(_('Too many output arguments for dramadah matrix.'));
      end
      [varargout{1:nargout('dramadah')}] = dramadah(nSize, patternType, className);
    case 'house'
      if (nargin() > 3)
        error(_('house matrix requires two additional arguments.'));
      end
      if nargin() < 3
        mode = 0;
      else
        mode = varargin{3};
      end
      
      nRows = varargin{2};
      [varargout{1:nargout('house')}] = house(nRows, mode);
    case 'binomial'
      if (nargin() < 2 || nargin() > 3)
        error(_('binomial matrix requires between 1 and 2 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() == 3
        classArg = varargin{3};
        % accept a class name string/char or a sample value (numeric/etc.)
        if ischar(classArg) || (isstring(classArg) && isscalar(classArg))
          className = classArg;
        else
          % convert non-string argument to its MATLAB class name
          className = class(classArg);
        end
      else
        className = 'double';
      end
      if nargout() > nargout('binomial')
        error(_('Too many output arguments for binomial matrix.'));
      end
      [varargout{1:nargout('binomial')}] = binomial(nSize, className);
    case 'cauchy'
      if (nargin() < 2 || nargin() > 4)
        error(_('cauchy matrix requires between 1 and 3 additional arguments.'));
      end
      xSeq = varargin{2};
      ySeq = [];
      if nargin() >= 3
        ySeq = varargin{3};
      end
      if nargin() == 4
        className = varargin{4};
      else
        className = 'double';
      end
      if nargout() > 1
        error(_('Too many output arguments for cauchy matrix.'));
      end
      [varargout{1:1}] = cauchy(xSeq, ySeq, className);
    case 'ris'
      if (nargin() < 2 || nargin() > 3)
        error(_('ris matrix requires between 1 and 2 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      if nargout() > nargout('ris')
        error(_('Too many output arguments for ris matrix.'));
      end
      [varargout{1:nargout('ris')}] = ris(nSize, className);
    case 'chebspec'
      if (nargin() < 2 || nargin() > 4)
        error(_('chebspec matrix requires between 1 and 3 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() >= 3
        trimOption = varargin{3};
      else
        trimOption = 0;
      end
      if nargin() == 4
        className = varargin{4};
      else
        className = 'double';
      end
      if nargout() > nargout('chebspec')
        error(_('Too many output arguments for chebspec matrix.'));
      end
      [varargout{1:nargout('chebspec')}] = chebspec(nSize, trimOption, className);
    case 'wilk'
      if (nargin() < 2 || nargin() > 3)
        error(_('wilk matrix requires between 1 and 2 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      if nargout() > nargout('wilk')
        error(_('Too many output arguments for wilk matrix.'));
      end
      [varargout{1:nargout('wilk')}] = wilk(nSize, className);
    case 'sampling'
      if (nargin() < 2 || nargin() > 3)
        error(_('sampling matrix requires between 1 and 2 additional arguments.'));
      end
      xSeq = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      [varargout{1:nargout('sampling')}] = sampling(xSeq, className);
    case 'ipjfact'
      if (nargin() < 2 || nargin() > 4)
        error(_('ipjfact matrix requires between 1 and 3 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() >= 3
        kind = varargin{3};
      else
        kind = [];
      end
      if nargin() == 4
        className = varargin{4};
      else
        className = 'double';
      end
      if nargout() > nargout('ipjfact')
        error(_('Too many output arguments for ipjfact matrix.'));
      end
      [varargout{1:nargout('ipjfact')}] = ipjfact(nSize, kind, className);
    case 'moler'
      if (nargin() < 2 || nargin() > 4)
        error(_('moler matrix requires between 1 and 3 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() >= 3
        alphaVal = varargin{3};
      else
        alphaVal = [];
      end
      if nargin() == 4
        className = varargin{4};
      else
        className = 'double';
      end
      if nargout() > nargout('moler')
        error(_('Too many output arguments for moler matrix.'));
      end
      [varargout{1:nargout('moler')}] = moler(nSize, alphaVal, className);
    case 'lotkin'
      if (nargin() < 2 || nargin() > 3)
        error(_('lotkin matrix requires between 1 and 2 additional arguments.'));
      end
      nSize = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      if nargout() > nargout('lotkin')
        error(_('Too many output arguments for lotkin matrix.'));
      end
      [varargout{1:nargout('lotkin')}] = lotkin(nSize, className);
    case 'chebvand'
      if (nargin() < 2 || nargin() > 4)
        error(_('chebvand matrix requires between 1 and 3 additional arguments.'));
      end
      numRows = varargin{2};
      % Resolve optional points sequence and optional class name:
      if nargin() == 4
        pointsSeq = varargin{3};
        className = varargin{4};
      elseif nargin() == 3
        thirdArg = varargin{3};
        if ischar(thirdArg) || (isstring(thirdArg) && isscalar(thirdArg))
          % third argument is a class name
          className = char(thirdArg);
          pointsSeq = [];
        else
          % third argument is the points sequence
          pointsSeq = thirdArg;
          className = 'double';
        end
      else
        pointsSeq = [];
        className = 'double';
      end
      if nargout() > nargout('chebvand')
        error(_('Too many output arguments for chebvand matrix.'));
      end
      [varargout{1:nargout('chebvand')}] = chebvand(numRows, pointsSeq, className);
    otherwise
      msg = sprintf(_('Unknown matrix name "%s".'), matname);
      error(msg);
    end
  end
  %=============================================================================
