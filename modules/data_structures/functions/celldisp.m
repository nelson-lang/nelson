%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function celldisp(c, s)
  narginchk(1, 2);
  nargoutchk(0, 0);
  mustBeA(c, 'cell', 1);
  
  if (nargin == 2)
    mustBeTextScalar(s, 2);
    s = convertStringsToChars(s);   
  else
    s = inputname(1);
  end
  
  fmt = format();
  isloose = (fmt.LineSpacing == "loose");
  
  if isempty(s)
    s = 'ans';
  end
  
  for i = 1:numel(c)
    if iscell(c{i}) && ~isempty(c{i})
      celldisp(c{i}, [s, subsCellPart(i, size(c))])
    else
      blankLine(isloose)
      disp([s, subsCellPart(i, size(c)) ' ='])
      blankLine(isloose)
      if isempty(c{i})
        if iscell(c{i})
          disp('     {}')
        elseif ischar(c{i})
          disp('     ''''')
        elseif isnumeric(c{i})
          disp('     []')
        else
          [m, n] = size(c{i});
          fprintf([_('%0.f-by-%0.f %s'), newline], m, n, class(c{i}));
        end
      else
        disp(c{i})
      end
      blankLine(isloose);
    end
  end
end
%=============================================================================
function blankLine(isloose)
  if (isloose)
    disp(' ')
  end 
end 
%=============================================================================
function s = subsCellPart(i, siz)
  if (length(siz) == 2 && any(any(siz == 1)))
    v = cell(1, 1);
  else
    v = cell(size(siz));
  end
  
  [v{1:end}] = ind2sub(siz, i);
  
  s = ['{' int2str(v{1})];
  for i = 2:length(v)
    s = [s ',' int2str(v{i})];
  end
  s = [s '}'];
end
%=============================================================================
