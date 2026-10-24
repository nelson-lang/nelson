%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
root = tempdir();
modules = fullpath(fullfile(nelsonroot(), 'modules'));
entry = [root, '/fingerprint_entry.m'];
base = [modules, '/elementary_functions/functions'];
files(1) = struct('source', entry, 'root', root, 'relative', 'fingerprint_entry.m', ...
  'module', '', 'representation', 'bytecode', 'sha256', repmat('1', 1, 64));
files(2) = struct('source', [base, '/first/same.m'], 'root', [base, '/first'], 'relative', 'same.m', ...
  'module', 'elementary_functions', 'representation', 'runtime-source', 'sha256', repmat('2', 1, 64));
files(3) = files(2);
files(3).source = [base, '/second/same.m'];
files(3).root = [base, '/second'];
files(3).sha256 = repmat('3', 1, 64);
plan = struct('files', files, 'entry', entry, 'searchRoots', {{root}}, ...
  'entryKind', 'function', 'engineFingerprint', repmat('4', 1, 64), ...
  'builtinCatalogDigest', repmat('5', 1, 64), 'dynamicFallback', false);
original = __nelsonc_source_fingerprint__(plan, '', '');
changed = plan;
changed.files(2).sha256 = plan.files(3).sha256;
changed.files(3).sha256 = plan.files(2).sha256;
asserts.isfalse(strcmp(__nelsonc_source_fingerprint__(changed, '', ''), original));
reordered = plan;
reordered.files = plan.files([3, 1, 2]);
asserts.isequal(__nelsonc_source_fingerprint__(reordered, '', ''), original);
changed = plan;
changed.files(1).timestamp = 'DIFFERENT';
asserts.isequal(__nelsonc_source_fingerprint__(changed, '', ''), original);
asserts.isfalse(strcmp(__nelsonc_source_fingerprint__(plan, 'new help', ''), original));
asserts.isfalse(strcmp(__nelsonc_source_fingerprint__(plan, '', repmat('6', 1, 64)), original));
