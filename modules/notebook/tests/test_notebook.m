%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('notebook'), 1);
asserts.isequal(nargout('notebook'), 1);
%=============================================================================
% A plain .m script runs as a single cell.
f1 = [tempdir(), 'test_ls_plain_', createGUID(), '.m'];
fw = fopen(f1, 'wt');
fwrite(fw, 'ls_plain_var = 40 + 2;');
fclose(fw);
tf = notebook(f1);
asserts.istrue(tf);
asserts.isequal(ls_plain_var, 42);
%=============================================================================
% A marked Notebook .m runs only its [code] sections (text/heading skipped).
f2 = [tempdir(), 'test_ls_marked_', createGUID(), '.m'];
content = sprintf('%%%% [text]\nthis text is never evaluated as code\n%%%% [code]\nls_marked_var = 6 * 7;\n');
fw = fopen(f2, 'wt');
fwrite(fw, content);
fclose(fw);
notebook(f2);
asserts.isequal(ls_marked_var, 42);
%=============================================================================
% A native .nlx container: evaluate the code cells of document.json.
d = [tempdir(), 'test_ls_nlx_', createGUID()];
mkdir(d);
fw = fopen([d, '/document.json'], 'wt');
fwrite(fw, '{"blocks":[{"type":"heading","props":{}},{"type":"codecell","props":{"code":"ls_nlx_var = 21 * 2;"}}]}');
fclose(fw);
fw = fopen([d, '/manifest.json'], 'wt');
fwrite(fw, '{"format":"nelson-notebook","version":1}');
fclose(fw);
nlx = [tempdir(), 'test_ls_', createGUID(), '.nlx'];
odir = pwd();
cd(d);
zip(nlx, '*.json');
cd(odir);
notebook(nlx);
asserts.isequal(ls_nlx_var, 42);
%=============================================================================
% run() on a .nlx delegates to the notebook module.
run(nlx);
asserts.isequal(ls_nlx_var, 42);
%=============================================================================
% A missing document is an error.
R = false;
try
  notebook([tempdir(), createGUID(), '.nlx']);
catch
  R = true;
end
asserts.istrue(R);
%=============================================================================
