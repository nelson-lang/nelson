%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% NELSON_NMM_REGISTRY_REUSE=1: a remote registry already fetched by this
% process is served from its verified cached copy instead of being fetched
% again. Without it, every query fetches the registry.
% A file:// URL goes through the remote (websave) code path, offline.
%=============================================================================
skip_testsuite(~ismodule('webtools'), 'webtools module is not installed.');
testRoot = [tempname(), '_nmm_reuse/'];
mkdir(testRoot);
registryFile = [testRoot, 'registry.json'];
registryUrl = ['file:///', strrep(registryFile, '\', '/')];
onePackage = '{"packages":[{"name":"reuse_a","version":"1.0.0","platforms":["all"],"source":"https://h/a.nmz"}]}';
twoPackages = '{"packages":[{"name":"reuse_a","version":"1.0.0","platforms":["all"],"source":"https://h/a.nmz"},{"name":"reuse_b","version":"1.0.0","platforms":["all"],"source":"https://h/b.nmz"}]}';
previousRegistry = getenv('NELSON_NMM_REGISTRY');
previousAllowUnsigned = getenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED');
previousReuse = getenv('NELSON_NMM_REGISTRY_REUSE');
setenv('NELSON_NMM_REGISTRY', registryUrl);
setenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED', '1');
counts = [];
warning('off', 'Nelson:nmm:RegistryUnsigned');
try
  setenv('NELSON_NMM_REGISTRY_REUSE', '');
  filewrite(registryFile, onePackage);
  report = nmm('registry', 'list');
  counts(end + 1) = numel(report.packages);
  filewrite(registryFile, twoPackages);
  % reuse requested: the copy fetched above is served, the change is not seen
  setenv('NELSON_NMM_REGISTRY_REUSE', '1');
  report = nmm('registry', 'list');
  counts(end + 1) = numel(report.packages);
  % reuse not requested: fetched again
  setenv('NELSON_NMM_REGISTRY_REUSE', '');
  report = nmm('registry', 'list');
  counts(end + 1) = numel(report.packages);
catch e
  counts = e.message;
end
warning('on', 'Nelson:nmm:RegistryUnsigned');
setenv('NELSON_NMM_REGISTRY', previousRegistry);
setenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED', previousAllowUnsigned);
setenv('NELSON_NMM_REGISTRY_REUSE', previousReuse);
rmdir(testRoot, 's');
assert_isequal(counts, [1, 1, 2]);
%=============================================================================
