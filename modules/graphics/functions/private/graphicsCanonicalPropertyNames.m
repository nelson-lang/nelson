%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function propertiesList = graphicsCanonicalPropertyNames(propertiesList, GOType)
  % Resolve abbreviated property names in a name/value list to their canonical
  % form (e.g. 'FaceC' -> 'FaceColor'), so a wrapper that looks a property up by
  % its canonical name handles abbreviations exactly as the graphics object
  % construction does. The resolution is the shared C++ one (shortest
  % unambiguous leading substring); unknown or ambiguous names are left
  % untouched so the existing error paths still report them.
  for k = 1:2:length(propertiesList)
    name = propertiesList{k};
    if ischar(name)
      canonical = canonicalGraphicsProperty(GOType, name);
      if ~isempty(canonical)
        propertiesList{k} = canonical;
      end
    end
  end
end
%=============================================================================
