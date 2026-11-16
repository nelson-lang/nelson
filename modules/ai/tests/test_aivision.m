%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% base64 of known byte sequences, via the image attachment helper
tmp = [tempname(), '.bin'];
fid = fopen(tmp, 'wb');
fwrite(fid, uint8('Man'), 'uint8');
fclose(fid);
opts = nelson.ai.attachImage(struct(), tmp);
assert_isequal(opts.Images{1}, 'TWFu');
rmfile(tmp);
%=============================================================================
tmp = [tempname(), '.bin'];
fid = fopen(tmp, 'wb');
fwrite(fid, uint8('Ma'), 'uint8');
fclose(fid);
opts = nelson.ai.attachImage(struct(), tmp);
assert_isequal(opts.Images{1}, 'TWE=');
rmfile(tmp);
%=============================================================================
tmp = [tempname(), '.bin'];
fid = fopen(tmp, 'wb');
fwrite(fid, uint8('M'), 'uint8');
fclose(fid);
opts = nelson.ai.attachImage(struct(), tmp);
assert_isequal(opts.Images{1}, 'TQ==');
rmfile(tmp);
%=============================================================================
% a missing file leaves options unchanged
opts = nelson.ai.attachImage(struct('Model', 'x'), [tempname(), '.none']);
assert_isfalse(isfield(opts, 'Images'));
%=============================================================================
% a longer byte sequence round-trips to a base64 string of the expected length
payload = uint8(0:199);
tmp = [tempname(), '.bin'];
fid = fopen(tmp, 'wb');
fwrite(fid, payload, 'uint8');
fclose(fid);
opts = nelson.ai.attachImage(struct(), tmp);
assert_isequal(numel(opts.Images{1}), 4 * ceil(numel(payload) / 3));
rmfile(tmp);
%=============================================================================
