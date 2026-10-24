function graphics_io_run_help_example(code, filename, index)
  %=============================================================================
  % Copyright (c) 2016-present Allan CORNET (Nelson)
  %=============================================================================
  % This file is part of Nelson.
  %=============================================================================
  % LICENCE_BLOCK_BEGIN
  % SPDX-License-Identifier: LGPL-3.0-or-later
  % LICENCE_BLOCK_END
  %=============================================================================
  try
    evalc(code);
  catch exception
    error(_('Help example failed in "%s" at example %d: %s'), ...
          filename, index, exception.message);
  end
  %=============================================================================
