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
      error('Nelson:elementary_functions:wrongValue1Argument', 'Wrong value for #%d argument.', 1);
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
        error('Nelson:elementary_functions:circulMatrixRequiresSecondArgument', 'Wrong number of input arguments.');
      end
      x = varargin{2};
      if nargout() > nargout('circul')
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('circul')}] = circul(x);
    case 'grcar'
      if (nargin() < 2 || nargin() > 4)
        error('Nelson:elementary_functions:grcarMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('grcar')}] = grcar(n, upperBand, className);
    case 'minij'
      if (nargin() ~= 2)
        error('Nelson:elementary_functions:minijMatrixRequiresSecondArgument', 'Wrong number of input arguments.');
      end
      nSize = varargin{2};
      if nargout() > nargout('minij')
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('minij')}] = minij(nSize, 'double');
    case 'dramadah'
      if (nargin() < 2 || nargin() > 4)
        error('Nelson:elementary_functions:dramadahMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('dramadah')}] = dramadah(nSize, patternType, className);
    case 'house'
      if (nargin() > 3)
        error('Nelson:elementary_functions:houseMatrixRequiresTwoAdditional', 'Wrong number of input arguments.');
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
        error('Nelson:elementary_functions:binomialMatrixRequiresBetween1', 'Wrong number of input arguments.');
      end
      nSize = varargin{2};
      if nargin() == 3
        classArg = varargin{3};
        % accept a class name string/char or a sample value (numeric/etc.)
        if ischar(classArg) || (isstring(classArg) && isscalar(classArg))
          className = classArg;
        else
          className = class(classArg);
        end
      else
        className = 'double';
      end
      if nargout() > nargout('binomial')
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('binomial')}] = binomial(nSize, className);
    case 'cauchy'
      if (nargin() < 2 || nargin() > 4)
        error('Nelson:elementary_functions:cauchyMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:1}] = cauchy(xSeq, ySeq, className);
    case 'ris'
      if (nargin() < 2 || nargin() > 3)
        error('Nelson:elementary_functions:risMatrixRequiresBetween1', 'Wrong number of input arguments.');
      end
      nSize = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      if nargout() > nargout('ris')
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('ris')}] = ris(nSize, className);
    case 'chebspec'
      if (nargin() < 2 || nargin() > 4)
        error('Nelson:elementary_functions:chebspecMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('chebspec')}] = chebspec(nSize, trimOption, className);
    case 'wilk'
      if (nargin() < 2 || nargin() > 3)
        error('Nelson:elementary_functions:wilkMatrixRequiresBetween1', 'Wrong number of input arguments.');
      end
      nSize = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      if nargout() > nargout('wilk')
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('wilk')}] = wilk(nSize, className);
    case 'sampling'
      if (nargin() < 2 || nargin() > 3)
        error('Nelson:elementary_functions:samplingMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:elementary_functions:ipjfactMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('ipjfact')}] = ipjfact(nSize, kind, className);
    case 'moler'
      if (nargin() < 2 || nargin() > 4)
        error('Nelson:elementary_functions:molerMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('moler')}] = moler(nSize, alphaVal, className);
    case 'lotkin'
      if (nargin() < 2 || nargin() > 3)
        error('Nelson:elementary_functions:lotkinMatrixRequiresBetween1', 'Wrong number of input arguments.');
      end
      nSize = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      if nargout() > nargout('lotkin')
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('lotkin')}] = lotkin(nSize, className);
    case 'chebvand'
      if (nargin() < 2 || nargin() > 4)
        error('Nelson:elementary_functions:chebvandMatrixRequiresBetween1', 'Wrong number of input arguments.');
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
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:nargout('chebvand')}] = chebvand(numRows, pointsSeq, className);
    case 'lehmer'
      if (nargin() < 2 || nargin() > 3)
        error('Nelson:elementary_functions:lehmerMatrixRequiresBetween1', 'Wrong number of input arguments.');
      end
      nSize = varargin{2};
      if nargin() == 3
        className = varargin{3};
      else
        className = 'double';
      end
      if nargout() > 1
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      [varargout{1:1}] = lehmer(nSize, className);
    otherwise
      error('Nelson:gallery:unknownMatrixName', 'Unknown matrix name "%s".', matname);
  end
end
%=============================================================================
