%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('unzip'), -2);
asserts.isequal(nargout('unzip'), -1);
asserts.isequal(nargin('untar'), -2);
asserts.isequal(nargout('untar'), -1);
asserts.istrue(nelson.io.internal.isUrl('https://example.org/a.zip'));
asserts.isfalse(nelson.io.internal.isUrl('C:/data/a.zip'));
asserts.checkerror('nelson.io.zip.open(''https://example.org/a.zip'', ''w'')', ...
  'A URL can only be opened for reading (mode ''r'').', 'Nelson:io:urlReadOnly');
asserts.checkerror('nelson.io.gzip.fopen(''https://example.org/a.gz'', ''w'')', ...
  'A URL can only be opened for reading (mode ''r'').', 'Nelson:io:urlReadOnly');
%=============================================================================
% Downloads need the network: the test stops when the server cannot be reached.
URL = 'https://raw.githubusercontent.com/nelson-lang/nelson/master/modules/file_archiver/tests/test_zip.zip';
try
  z = nelson.io.zip.open(URL);
catch
  return
end
T = z.entries();
asserts.istrue(any(T.Name == "file_0.txt"));
z.close();
WORK = [tempdir(), createGUID()];
names = unzip(URL, WORK);
asserts.isequal(numel(names), height(T));
asserts.istrue(isfile([WORK, '/file_0.txt']));
rmdir(WORK, 's');
%=============================================================================
