%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function header = poheader(DOMAIN, LANGUAGE)
  domain_str = ['"Project-Id-Version: ', DOMAIN, '\n"'];
  header = {};
  header{1, 1} = 'msgid ""';
  header{2, 1} = 'msgstr ""';
  header{3, 1} = ['"Project-Id-Version: ', DOMAIN, '\n"'];
  header{4, 1} = ['"Language: ', LANGUAGE, '\n"'];
  header{5, 1} = '"MIME-Version: 1.0\n"';
  header{6, 1} = '"Content-Type: text/plain; charset=UTF-8\n"';
  header{7, 1} = '"Content-Transfer-Encoding: 8bit\n"';
  header{8, 1} = '';
end

