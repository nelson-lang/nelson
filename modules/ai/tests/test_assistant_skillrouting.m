%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Routing / audience regression for the assistant skill catalog. Deterministic:
% no model call, only the catalog and its trigger matching. Locks in that an
% end-user prompt no longer floods on the word "nelson" and never pulls in a
% contributor (Nelson-development) skill, and that the usage/nflow guides route.
%=============================================================================
% every catalog skill carries a valid audience and a real file
all = nelson.ai.skills();
assert_istrue(~isempty(all));
for k = 1:numel(all)
  assert_istrue(~isempty(all(k).name));
  assert_istrue(isfile(all(k).path));
  assert_istrue(any(strcmp(all(k).audience, {'user', 'contributor'})));
end
%=============================================================================
% audience assignments: end-user guides vs contributor (development) guides
assert_isequal(audienceOf(all, 'nelson-usage-language-basics'), 'user');
assert_isequal(audienceOf(all, 'nflow-assistant'), 'user');
assert_isequal(audienceOf(all, 'nelson-data-analysis-workflows'), 'user');
assert_isequal(audienceOf(all, 'nelson-core-native-builtins'), 'contributor');
assert_isequal(audienceOf(all, 'nelson-companion-mcp-workflows'), 'contributor');
% references source-tree examples/libraries -> not for the binary-only end user
assert_isequal(audienceOf(all, 'nflow-create-model'), 'contributor');
%=============================================================================
% the end-user usage skill exists and is discoverable by a plain usage prompt
plotMatch = nelson.ai.skills('comment tracer une figure avec plot ?');
assert_istrue(hasName(plotMatch, 'nelson-usage-language-basics'));
%=============================================================================
% the "nelson" flood is gone: a usage prompt that mentions Nelson no longer drags
% in the contributor guides (they used to match on the bare name token "nelson")
usageMatch = nelson.ai.skills('je veux utiliser nelson pour lire un fichier csv');
assert_isfalse(hasName(usageMatch, 'nelson-core-native-builtins'));
assert_isfalse(hasName(usageMatch, 'nelson-core-packaging'));
assert_isfalse(hasName(usageMatch, 'nelson-core-code-review'));
assert_isfalse(hasName(usageMatch, 'nelson-companion-mcp-workflows'));
%=============================================================================
% the live-assistant contract: after keeping only user-audience skills (what
% aiSkillContext injects), a usage prompt yields no contributor guide at all
userOnly = usageMatch(strcmp({usageMatch.audience}, 'user'));
for k = 1:numel(userOnly)
  assert_isequal(userOnly(k).audience, 'user');
end
%=============================================================================
% nflow routing still works through the curated triggers
nflowMatch = nelson.ai.skills('construire un modele nflow avec un gain et un scope');
assert_istrue(hasName(nflowMatch, 'nflow-assistant'));
%=============================================================================
function tf = hasName(skills, name)
  tf = ~isempty(skills) && any(strcmp({skills.name}, name));
end
%=============================================================================
function audience = audienceOf(skills, name)
  index = find(strcmp({skills.name}, name), 1);
  assert_istrue(~isempty(index));
  audience = skills(index).audience;
end
%=============================================================================
