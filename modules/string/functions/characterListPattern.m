%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = characterListPattern(characters, stopCharacter)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if isstring(characters)
    characters = char(characters);
  end
  if nargin == 1
    pat = pattern(['[', characterClass(characters), ']']);
  else
    if isstring(stopCharacter)
      stopCharacter = char(stopCharacter);
    end
    pat = pattern(['[', regexptranslate('escape', characters), '-', regexptranslate('escape', stopCharacter), ']']);
  end
end
%=============================================================================
function cls = characterClass(characters)
  cls = '';
  hasHyphen = false;
  for k = 1:length(characters)
    ch = characters(k);
    if ch == '-'
      hasHyphen = true;
    elseif any(ch == '\^[]')
      cls = [cls, '\', ch];
    else
      cls = [cls, ch];
    end
  end
  if hasHyphen
    cls = [cls, '\-'];
  end
end
%=============================================================================
