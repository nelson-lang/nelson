%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('nelson.ai.marketplace.install'), 1);
asserts.isequal(nargin('nelson.ai.marketplace.remove'), 1);
asserts.isequal(nargout('nelson.ai.marketplace.installed'), 1);
asserts.isequal(nargout('nelson.ai.marketplace.search'), 1);
%=============================================================================
% Build a minimal .nmz (a zip holding a SKILL.md) in a temporary folder.
skillName = 'test_marketplace_nmz';
root = [tempdir(), skillName];
if isfolder(root)
  rmdir(root, 's');
end
mkdir(root);
skillFile = [root, '/SKILL.md'];
fid = fopen(skillFile, 'wt');
fprintf(fid, '# Demo skill\n');
fclose(fid);
nmz = [root, '/', skillName, '.nmz'];
zip(nmz, skillFile);
asserts.istrue(isfile(nmz));
%=============================================================================
% install then list: the package shows up in the installed set.
nelson.ai.marketplace.install(nmz);
inst = nelson.ai.marketplace.installed();
asserts.istrue(isstruct(inst));
asserts.istrue(any(strcmp({inst.name}, skillName)));
%=============================================================================
% remove: the package is gone from the installed set.
nelson.ai.marketplace.remove(skillName);
inst = nelson.ai.marketplace.installed();
asserts.istrue(~any(strcmp({inst.name}, skillName)));
%=============================================================================
% removing a skill that is not installed raises a clear error.
removeFailed = false;
try
  nelson.ai.marketplace.remove(skillName);
catch
  removeFailed = true;
end
asserts.istrue(removeFailed);
%=============================================================================
% search tolerates an unreachable catalog: it returns an empty result, no error.
previousCatalog = getenv('NELSON_AI_SKILLS_CATALOG');
setenv('NELSON_AI_SKILLS_CATALOG', 'http://127.0.0.1:0/nope.json');
result = nelson.ai.marketplace.search('anything');
setenv('NELSON_AI_SKILLS_CATALOG', previousCatalog);
asserts.istrue(isstruct(result));
asserts.isequal(numel(result), 0);
%=============================================================================
rmdir(root, 's');
%=============================================================================
