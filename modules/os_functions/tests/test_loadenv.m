%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('loadenv'), 1);
assert_isequal(nargout('loadenv'), -1);
%=============================================================================
env_file = [modulepath('os_functions', 'tests'), '/sample.env'];
D = loadenv(env_file);
%=============================================================================
assert_isequal(class(D), 'dictionary');
REF = ["Key1";
    "Key2";
    "AWS_ACCESS_KEY_ID";
    "AWS_SECRET_ACCESS_KEY";
    "MW_WASB_SAS_TOKEN";
    "username";
    "password";
    "DB_NAME";
    "DB_USER";
    "DB_PASSWORD";
    "DB_DOMAIN";
    "DB_PORT";
    "TEMPORARY_DOWNLOAD";
    "会意; 會意"];
assert_isequal(D.keys, REF);
%=============================================================================
assert_isequal(getenv('Key1'), '');
loadenv(env_file);
assert_isequal(getenv('Key1'), 'Value1');
%=============================================================================
assert_isequal(getenv('会意; 會意'), 'OK');
%=============================================================================