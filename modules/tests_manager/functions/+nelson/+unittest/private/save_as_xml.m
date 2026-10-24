%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function res = save_as_xml(test_suites, xmlFileDestination)
  res = false;
  fp = fopen(xmlFileDestination, 'wt');
  if fp == -1
    return
  end
  eol = newline;
  fprintf(fp, ['<?xml version="1.0"?>', eol]);
  fprintf(fp, ['<testsuites tests="', int2str(test_suites.tests), '" disabled="', ...
    int2str(test_suites.disabled), '" time="', mat2str(test_suites.time, 2), ...
    '" errors="', int2str(test_suites.errors), '">', eol]);
  for test_suite = test_suites.tests_list'
    writeSuite(fp, test_suite, eol);
  end
  fprintf(fp, ['</testsuites>', eol]);
  fclose(fp);
  res = true;
end
