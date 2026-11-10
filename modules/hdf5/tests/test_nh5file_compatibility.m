%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
previous = cd(folder);
cleanup = onCleanup(@() cd(previous));
%=============================================================================
% a file saved by an older version (one chunk per compressed variable)
legacy = fullfile(modulepath('hdf5', 'tests'), 'h5save_schema_1.nh5');
copyfile(legacy, 'legacy.nh5');
ref = loadnh5('legacy.nh5');
m = nh5file('legacy.nh5');
asserts.isequal(m.Properties.Writable, false);
asserts.isequal(m.AC(2, 3:4, 2:2:6), ref.AC(2, 3:4, 2:2:6));
asserts.isequal(m.AD(:, 1, 1), ref.AD(:, 1, 1));
asserts.isequal(m.C(1:2, :), ref.C(1:2, :));
asserts.isequal(m.A(5, :), ref.A(5, :));
asserts.isequal(m.AN(3, 4), ref.AN(3, 4));
asserts.isequal(m.AO(1, 2), ref.AO(1, 2));
asserts.isequal(m.AZ, ref.AZ);
asserts.isequal(size(m, 'AT'), size(ref.AT));
%=============================================================================
% blocks written into an older file keep it readable by loadnh5
m.Properties.Writable = true;
m.AC(1, 1, 1) = -7;
m.C(2, 2) = 3 + 4i;
m.AD(1, 1:2, 1) = 'zz';
ref.AC(1, 1, 1) = -7;
ref.C(2, 2) = 3 + 4i;
ref.AD(1, 1:2, 1) = 'zz';
after = loadnh5('legacy.nh5');
asserts.isequal(after.AC, ref.AC);
asserts.isequal(after.C, ref.C);
asserts.isequal(after.AD, ref.AD);
asserts.isequal(after.AO, ref.AO);
%=============================================================================
% large variables are saved in chunks: loadnh5 and nh5file read the same values
x = reshape(1:1200 * 900, 1200, 900);
z = complex(x(1:500, 1:300), -x(1:500, 1:300));
s = single(x(1:700, :));
savenh5('chunked.nh5', 'x', 'z', 's');
asserts.istrue(isnh5file('chunked.nh5'));
back = loadnh5('chunked.nh5');
asserts.isequal(back.x, x);
asserts.isequal(back.z, z);
asserts.isequal(back.s, s);
n = nh5file('chunked.nh5', 'Writable', true);
asserts.isequal(n.x(1100:1200, 3:300:900), x(1100:1200, 3:300:900));
asserts.isequal(n.z(500, :), z(500, :));
n.x(600:601, 450:452) = zeros(2, 3);
x(600:601, 450:452) = 0;
asserts.isequal(loadnh5('chunked.nh5').x, x);
%=============================================================================
% the format markers are unchanged
asserts.isequal(h5readatt('chunked.nh5', '/', 'NELSON_schema'), int32(1));
asserts.isequal(h5readatt('chunked.nh5', '/x', 'NELSON_class'), 'double');
%=============================================================================
% other HDF5 files are not .nh5 files
other = fullfile(modulepath('hdf5', 'tests'), 'h5', 'example.h5');
o = nh5file(other);
asserts.checkerror('o.x', ['''', other, ''' is not a valid .nh5 file.']);
%=============================================================================
