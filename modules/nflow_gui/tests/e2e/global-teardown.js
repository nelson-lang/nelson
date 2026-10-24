// ============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default async function globalTeardown() {
  const pidFile = path.join(__dirname, 'static-server.pid');
  try {
    if (fs.existsSync(pidFile)) {
      const pid = Number(fs.readFileSync(pidFile, 'utf8'));
      if (pid && !Number.isNaN(pid)) {
        try { process.kill(pid); } catch (e) {}
      }
      fs.unlinkSync(pidFile);
    }
  } catch (e) {
    // ignore
  }
}
