%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Skill catalog URL for the marketplace. Overridable with the environment
% variable NELSON_AI_SKILLS_CATALOG. The catalog is a JSON array of entries
% { name, description, url } where url points to a .nmz package.
function url = catalogUrl()
  url = getenv('NELSON_AI_SKILLS_CATALOG');
  if isempty(url)
    url = 'https://nelson-lang.github.io/nelson-skills/catalog.json';
  end
end
%=============================================================================
