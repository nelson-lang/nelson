%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.argon2'), 16);
asserts.isequal(nargout('crypto.argon2'), 1);
%=============================================================================
% RFC 9106 section 5 test vectors: t = 3, m = 32 KiB, p = 4, tag 32 bytes
password = uint8(repmat(1, 1, 32));
salt = uint8(repmat(2, 1, 16));
secret = uint8(repmat(3, 1, 8));
ad = uint8(repmat(4, 1, 12));
common = {'Memory', 32, 'Passes', 3, 'Lanes', 4, 'Length', 32, 'Key', secret, 'AssociatedData', ad};
R = crypto.argon2(password, salt, 'Variant', 'argon2d', common{:});
asserts.isequal(R, '512b391b6f1162975371d30919734294f868e3be3984f3c1a13a4db9fabe4acb');
asserts.isequal(class(R), 'char');
R = crypto.argon2(password, salt, 'Variant', 'argon2i', common{:});
asserts.isequal(R, 'c814d9d1dc7f37aa13f0d77f2494bda1c8de6b016dd388d29952a4c4672b6ce8');
R = crypto.argon2(password, salt, 'Variant', 'argon2id', common{:});
asserts.isequal(R, '0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659');
R = crypto.argon2(password, salt, 'variant', "ARGON2ID", common{:});
asserts.isequal(R, '0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659');
%=============================================================================
% argon2id is the default variant; text password and salt use their UTF-8 bytes
R1 = crypto.argon2('correct horse battery staple', 'salt-of-16-bytes', 'Memory', 64, 'Passes', 2);
R2 = crypto.argon2(uint8('correct horse battery staple'), uint8('salt-of-16-bytes'), 'Variant', 'argon2id', 'Memory', 64, 'Passes', 2);
asserts.isequal(R1, R2);
asserts.isequal(length(R1), 64);
R3 = crypto.argon2('correct horse battery staple', 'salt-of-16-bytes', 'Memory', 64, 'Passes', 2, 'Length', 16);
asserts.isequal(length(R3), 32);
asserts.isfalse(strcmp(R3, R1(1:32)));
asserts.isfalse(strcmp(R1, crypto.argon2('correct horse battery staple', 'other-salt-16byt', 'Memory', 64, 'Passes', 2)));
asserts.isfalse(strcmp(R1, crypto.argon2('correct horse battery staple', 'salt-of-16-bytes', 'Memory', 64, 'Passes', 3)));
%=============================================================================
% argument errors
asserts.checkerror('crypto.argon2(''pwd'')', _('Not enough input arguments.'));
asserts.checkerror('crypto.argon2(''pwd'', ''short'')', _('Argon2 salt must be at least 8 bytes.'), 'Nelson:core:argon2SaltSizeExpected');
asserts.checkerror('crypto.argon2(''pwd'', ''salt-of-16-bytes'', ''Memory'')', _('Argon2 options must be name/value pairs.'), 'Nelson:core:argon2OptionValueExpected');
asserts.checkerror('crypto.argon2(''pwd'', ''salt-of-16-bytes'', ''Rounds'', 3)', sprintf(_('Unknown Argon2 option: %s'), 'Rounds'), 'Nelson:core:argon2OptionExpected');
asserts.checkerror('crypto.argon2(''pwd'', ''salt-of-16-bytes'', ''Variant'', ''scrypt'')', sprintf(_('Unsupported Argon2 variant: %s (''argon2id'', ''argon2i'' or ''argon2d'' expected).'), 'scrypt'), 'Nelson:core:argon2VariantExpected');
asserts.checkerror('crypto.argon2(''pwd'', ''salt-of-16-bytes'', ''Passes'', 0)', sprintf(_('Argon2 %s must be an integer between %d and %d.'), 'Passes', 1, 2147483647), 'Nelson:core:argon2ParameterInvalid');
asserts.checkerror('crypto.argon2(''pwd'', ''salt-of-16-bytes'', ''Memory'', 16, ''Lanes'', 4)', _('Argon2 memory (KiB) must be at least 8 times the number of lanes.'), 'Nelson:core:argon2MemoryExpected');
%=============================================================================
