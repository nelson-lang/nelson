%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% With no NELSON_NMM_REGISTRY set and no local registry.json in usermodulesdir(),
% nmm falls back to the official Nelson package registry, so a fresh install can
% browse and install packages with no configuration. An explicit override and a
% local registry.json still take precedence.
%=============================================================================
official = 'https://raw.githubusercontent.com/nelson-lang/nelson-modules-registry/master/registry.json';

previousExternal = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousRegistry = getenv('NELSON_NMM_REGISTRY');
testRoot = [tempdir(), 'nmm_default_registry_', createGUID(), '/'];
mkdir(testRoot);
setenv('NELSON_EXTERNAL_MODULES_PATH', testRoot);
setenv('NELSON_NMM_REGISTRY');

% 1) nothing configured -> official registry
asserts.isequal(nmm('config').registry, official);
asserts.isequal(nmm('doctor').registry, official);

% 2) an explicit NELSON_NMM_REGISTRY override wins
setenv('NELSON_NMM_REGISTRY', 'https://example.org/custom/registry.json');
asserts.isequal(nmm('config').registry, 'https://example.org/custom/registry.json');

% 3) a local registry.json in usermodulesdir() takes precedence over the default
setenv('NELSON_NMM_REGISTRY');
localRegistry = [usermodulesdir(), 'registry.json'];
filewrite(localRegistry, '{"packages": []}');
asserts.isequal(nmm('config').registry, localRegistry);
rmfile(localRegistry);

% back to the official default once the local file is gone
asserts.isequal(nmm('config').registry, official);

if isempty(previousExternal)
  setenv('NELSON_EXTERNAL_MODULES_PATH');
else
  setenv('NELSON_EXTERNAL_MODULES_PATH', previousExternal);
end
if isempty(previousRegistry)
  setenv('NELSON_NMM_REGISTRY');
else
  setenv('NELSON_NMM_REGISTRY', previousRegistry);
end
%=============================================================================
