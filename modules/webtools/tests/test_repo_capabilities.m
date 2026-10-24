%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
version = repo('version');
asserts.istrue(ischar(version));
asserts.isfalse(isempty(version));
asserts.istrue(contains(version, '.'));
%=============================================================================
capabilities = repo('capabilities');
asserts.istrue(isstruct(capabilities));
asserts.isequal(fieldnames(capabilities), {'version';
                                           'threads';
                                           'https';
                                           'ssh';
                                           'nsec';
                                           'https_backend';
                                           'ssh_backend'});
asserts.isequal(capabilities.version, version);
asserts.istrue(islogical(capabilities.threads));
asserts.istrue(islogical(capabilities.https));
asserts.istrue(islogical(capabilities.ssh));
asserts.istrue(islogical(capabilities.nsec));
asserts.istrue(ischar(capabilities.https_backend));
asserts.istrue(ischar(capabilities.ssh_backend));
if ~capabilities.ssh
  command = ['repo(''clone'', ''git@example.invalid:repo.git'', ''', tempname(), ''')'];
  asserts.checkerror(command, 'SSH support is not available in this Nelson build of libgit2.');
  command = ['repo(''clone'', ''git@example.invalid:repo.git'', ''', tempname(), ''', ''UseAgent'', true)'];
  asserts.checkerror(command, 'SSH support is not available in this Nelson build of libgit2.');
end
%=============================================================================
