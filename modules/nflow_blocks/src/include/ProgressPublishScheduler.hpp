//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstddef>
#include <cstdint>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Wall-clock publication scheduler.
    //
    // Decides WHEN a running simulation emits an interactive progress update,
    // from elapsed monotonic wall time rather than the step count, so the
    // cadence does not depend on dt:
    //   - a fast run that finishes within one period emits no interim update
    //     (only the caller's terminal Done/Stop/Error result);
    //   - a long run emits at most one update per period (~10 Hz by default);
    //   - a real-time run lands near the same rate regardless of dt.
    //
    // Time is a monotonic nanosecond count passed by the caller, so the class is
    // testable without a real clock. A terminal publish is forced by the caller
    // and not gated here. Each period is measured from the instant the previous
    // update was actually published (not from a fixed grid), so there is no
    // cumulative drift.
    class ProgressPublishScheduler
    {
    public:
        static constexpr long long kDefaultPeriodNs = 100000000LL; // 100 ms

        explicit ProgressPublishScheduler(long long periodNs = kDefaultPeriodNs)
            : periodNs_(periodNs > 0 ? periodNs : 1)
        {
        }

        // Anchor the first period at nowNs (call once at loop entry).
        void
        start(long long nowNs)
        {
            lastNs_ = nowNs;
            started_ = true;
        }

        // True when a full period has elapsed since the last published update
        // (or since start()).
        bool
        due(long long nowNs) const
        {
            return started_ && (nowNs - lastNs_) >= periodNs_;
        }

        // The next period is measured from nowNs.
        void
        markPublished(long long nowNs)
        {
            lastNs_ = nowNs;
            started_ = true;
        }

        long long
        periodNs() const
        {
            return periodNs_;
        }

    private:
        long long periodNs_;
        long long lastNs_ = 0;
        bool started_ = false;
    };
    //=============================================================================
    // Per-run progress-delta bookkeeping. One instance per simulation run (never
    // shared between concurrent runs), owned by runSimulation and passed by
    // pointer into the step loops:
    //   - scheduler:            wall-clock cadence gate (above);
    //   - sequence:             strictly increasing id stamped on each partial
    //                           payload, so the UI can detect duplicate / stale /
    //                           missing frames;
    //   - lastPublishedSample:  index one past the last sample already published;
    //                           the next delta serializes [lastPublishedSample,
    //                           time.size()), so each sample is serialized at
    //                           most once across the whole run;
    //   - dashboardPublishedLen: per-block (nid-indexed) count of dashboardScope
    //                           samples already published, giving dashboard
    //                           streams the same at-most-once guarantee.
    struct ProgressDeltaSession
    {
        ProgressPublishScheduler scheduler;
        long long sequence = 0;
        int lastPublishedSample = 0;
        std::vector<std::size_t> dashboardPublishedLen;
    };
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
