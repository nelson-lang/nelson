//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Post-route nudging / separation pass; native mirror of router/nudge.ts.
// See the TS module for the full design commentary; the two implementations
// are kept behaviorally identical (locked by the mirrored doctest cases).
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "RouterNudge.hpp"
#include <algorithm>
#include <cmath>
#include <map>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        //=============================================================================
        struct ChannelSegment
        {
            NudgeConn* conn = nullptr;
            int segIndex = 0; ///< points[segIndex] -> points[segIndex + 1]
            char orient = 'H';
            int track = 0; ///< y for H, x for V
            int lo = 0;
            int hi = 0;
            int entryCoord = 0; ///< perpendicular coord of preceding bend's far end
            int exitCoord = 0; ///< perpendicular coord of following bend's far end
            std::string sourceKey; ///< from:fromIndex, shared-source grouping
            std::string connKey;
        };
        //=============================================================================
        struct Member
        {
            std::vector<ChannelSegment*> segments;
            int entryCoord = 0;
            int exitCoord = 0;
            std::string connKey;
        };
        //=============================================================================
        std::string
        connKeyOf(const NudgeConn& c, int idx)
        {
            return c.from + ':' + std::to_string(c.fromIndex) + "->" + c.to + ':'
                + std::to_string(c.toIndex) + ':' + std::to_string(idx);
        }
        //=============================================================================
        std::vector<ChannelSegment>
        collectSegments(std::vector<NudgeConn>& connections)
        {
            std::vector<ChannelSegment> out;
            for (int connIdx = 0; connIdx < static_cast<int>(connections.size()); ++connIdx) {
                auto& conn = connections[connIdx];
                if (!conn.points || conn.points->size() < 4) {
                    continue; // no interior segment
                }
                // Pinned (user-anchored) wires are immovable for this pass.
                if (conn.pinned) {
                    continue;
                }
                auto& pts = *conn.points;
                int segCount = static_cast<int>(pts.size()) - 1;
                for (int i = 1; i < segCount - 1; ++i) {
                    const RPoint& a = pts[i];
                    const RPoint& b = pts[i + 1];
                    if (a.x == b.x && a.y == b.y) {
                        continue;
                    }
                    char orient = (a.x == b.x) ? 'V' : ((a.y == b.y) ? 'H' : 0);
                    if (!orient) {
                        continue;
                    }
                    const RPoint& prev = pts[i - 1];
                    const RPoint& next = pts[i + 2];
                    ChannelSegment seg;
                    seg.conn = &conn;
                    seg.segIndex = i;
                    seg.orient = orient;
                    seg.sourceKey = conn.from + ':' + std::to_string(conn.fromIndex);
                    seg.connKey = connKeyOf(conn, connIdx);
                    if (orient == 'H') {
                        seg.track = a.y;
                        seg.lo = (std::min)(a.x, b.x);
                        seg.hi = (std::max)(a.x, b.x);
                        seg.entryCoord = prev.y;
                        seg.exitCoord = next.y;
                    } else {
                        seg.track = a.x;
                        seg.lo = (std::min)(a.y, b.y);
                        seg.hi = (std::max)(a.y, b.y);
                        seg.entryCoord = prev.x;
                        seg.exitCoord = next.x;
                    }
                    out.push_back(seg);
                }
            }
            return out;
        }
        //=============================================================================
        // Union-find channel clustering: same orient+track, overlapping intervals.
        std::vector<std::vector<ChannelSegment*>>
        buildChannels(std::vector<ChannelSegment>& segments)
        {
            std::map<std::pair<char, int>, std::vector<ChannelSegment*>> byTrack;
            for (auto& seg : segments) {
                byTrack[{ seg.orient, seg.track }].push_back(&seg);
            }
            std::vector<std::vector<ChannelSegment*>> channels;
            for (auto& [key, list] : byTrack) {
                (void)key;
                if (list.size() < 2) {
                    continue;
                }
                std::sort(list.begin(), list.end(), [](ChannelSegment* a, ChannelSegment* b) {
                    if (a->lo != b->lo) {
                        return a->lo < b->lo;
                    }
                    if (a->hi != b->hi) {
                        return a->hi < b->hi;
                    }
                    return a->connKey < b->connKey;
                });
                std::vector<ChannelSegment*> cluster { list[0] };
                int clusterHi = list[0]->hi;
                for (size_t i = 1; i < list.size(); ++i) {
                    ChannelSegment* seg = list[i];
                    if (seg->lo < clusterHi) {
                        cluster.push_back(seg);
                        clusterHi = (std::max)(clusterHi, seg->hi);
                    } else {
                        if (cluster.size() > 1) {
                            channels.push_back(cluster);
                        }
                        cluster = { seg };
                        clusterHi = seg->hi;
                    }
                }
                if (cluster.size() > 1) {
                    channels.push_back(cluster);
                }
            }
            return channels;
        }
        //=============================================================================
        // Group a channel's segments into members (shared-source siblings merge).
        std::vector<Member>
        buildMembers(std::vector<ChannelSegment*>& cluster)
        {
            std::map<std::string, std::vector<ChannelSegment*>> bySource;
            for (auto* seg : cluster) {
                bySource[seg->sourceKey].push_back(seg);
            }
            std::vector<Member> members;
            for (auto& [key, segs] : bySource) {
                (void)key;
                std::sort(segs.begin(), segs.end(),
                    [](ChannelSegment* a, ChannelSegment* b) { return a->connKey < b->connKey; });
                Member m;
                m.segments = segs;
                m.entryCoord = segs[0]->entryCoord;
                m.exitCoord = segs[0]->exitCoord;
                for (auto* s : segs) {
                    m.entryCoord = (std::min)(m.entryCoord, s->entryCoord);
                    m.exitCoord = (std::min)(m.exitCoord, s->exitCoord);
                }
                m.connKey = segs[0]->connKey;
                members.push_back(m);
            }
            // Crossing-minimizing order (entryCoord, exitCoord, connKey).
            std::sort(members.begin(), members.end(), [](const Member& a, const Member& b) {
                if (a.entryCoord != b.entryCoord) {
                    return a.entryCoord < b.entryCoord;
                }
                if (a.exitCoord != b.exitCoord) {
                    return a.exitCoord < b.exitCoord;
                }
                return a.connKey < b.connKey;
            });
            return members;
        }
        //=============================================================================
        bool
        segmentCrossesRect(char orient, int track, int lo, int hi, const NudgeRect& rect)
        {
            if (orient == 'H') {
                return track > rect.top && track < rect.bottom && hi > rect.left && lo < rect.right;
            }
            return track > rect.left && track < rect.right && hi > rect.top && lo < rect.bottom;
        }
        //=============================================================================
        struct TouchedPoint
        {
            RPoint* pt;
            char axis;
            int old;
        };
        //=============================================================================
        void
        applyMemberOffset(Member& member, int offset, std::vector<TouchedPoint>& touched)
        {
            std::vector<RPoint*> seen;
            for (auto* seg : member.segments) {
                auto& pts = *seg->conn->points;
                char axis = (seg->orient == 'H') ? 'y' : 'x';
                RPoint* cand[2] = { &pts[seg->segIndex], &pts[seg->segIndex + 1] };
                for (RPoint* pt : cand) {
                    if (std::find(seen.begin(), seen.end(), pt) != seen.end()) {
                        continue;
                    }
                    seen.push_back(pt);
                    int& coord = (axis == 'y') ? pt->y : pt->x;
                    touched.push_back({ pt, axis, coord });
                    coord += offset;
                }
            }
        }
        //=============================================================================
        void
        undoOffsets(std::vector<TouchedPoint>& touched)
        {
            for (auto& t : touched) {
                int& coord = (t.axis == 'y') ? t.pt->y : t.pt->x;
                coord = t.old;
            }
            touched.clear();
        }
        //=============================================================================
        // First/last segments must keep direction and a minimal stub length;
        // compared end-for-end (guard bends may be spliced in).
        bool
        stubsIntact(const NudgeConn& conn, const std::vector<RPoint>& original, int spacing)
        {
            const auto& pts = *conn.points;
            if (pts.size() < 2 || original.size() < 2) {
                return false;
            }
            auto sgn = [](int v) { return (v > 0) - (v < 0); };
            auto check = [&](const RPoint& oa, const RPoint& ob, const RPoint& na,
                             const RPoint& nb) {
                if (sgn(ob.x - oa.x) != sgn(nb.x - na.x) || sgn(ob.y - oa.y) != sgn(nb.y - na.y)) {
                    return false;
                }
                int len = std::abs(nb.x - na.x) + std::abs(nb.y - na.y);
                return len >= spacing;
            };
            return check(original[0], original[1], pts[0], pts[1])
                && check(original[original.size() - 2], original[original.size() - 1],
                    pts[pts.size() - 2], pts[pts.size() - 1]);
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    int
    nudgeConnections(
        std::vector<NudgeConn>& connections, const std::vector<NudgeRect>& rects, int spacing)
    {
        if (connections.size() < 2) {
            return 0;
        }
        auto segments = collectSegments(connections);
        auto channels = buildChannels(segments);
        std::sort(channels.begin(), channels.end(),
            [](const std::vector<ChannelSegment*>& a, const std::vector<ChannelSegment*>& b) {
                auto keyOf = [](const std::vector<ChannelSegment*>& c) {
                    return std::string(1, c[0]->orient) + ':' + std::to_string(c[0]->track) + ':'
                        + std::to_string(c[0]->lo);
                };
                return keyOf(a) < keyOf(b);
            });

        int moved = 0;

        auto currentGeom = [](const ChannelSegment& seg, int& track, int& lo, int& hi) {
            const auto& pts = *seg.conn->points;
            const RPoint& a = pts[seg.segIndex];
            const RPoint& b = pts[seg.segIndex + 1];
            if (seg.orient == 'H') {
                track = a.y;
                lo = (std::min)(a.x, b.x);
                hi = (std::max)(a.x, b.x);
            } else {
                track = a.x;
                lo = (std::min)(a.y, b.y);
                hi = (std::max)(a.y, b.y);
            }
        };

        auto validateMember
            = [&](Member& member, const std::map<NudgeConn*, std::vector<RPoint>>& originals) {
                  for (const auto& [conn, original] : originals) {
                      if (!stubsIntact(*conn, original, spacing)) {
                          return false;
                      }
                  }
                  for (auto* seg : member.segments) {
                      int track, lo, hi;
                      currentGeom(*seg, track, lo, hi);
                      for (const auto& r : rects) {
                          if (segmentCrossesRect(seg->orient, track, lo, hi, r)) {
                              return false;
                          }
                      }
                      for (const auto& other : segments) {
                          if (other.conn == seg->conn) {
                              continue;
                          }
                          if (other.sourceKey == seg->sourceKey) {
                              continue;
                          }
                          bool inMember = false;
                          for (auto* ms : member.segments) {
                              if (ms == &other) {
                                  inMember = true;
                                  break;
                              }
                          }
                          if (inMember || other.orient != seg->orient) {
                              continue;
                          }
                          int otrack, olo, ohi;
                          currentGeom(other, otrack, olo, ohi);
                          if (otrack != track) {
                              continue;
                          }
                          if ((std::min)(hi, ohi) - (std::max)(lo, olo) > 0) {
                              return false;
                          }
                      }
                  }
                  return true;
              };

        for (auto& cluster : channels) {
            auto members = buildMembers(cluster);
            if (members.size() < 2) {
                continue; // single net (or shared source), by design
            }
            int mid = static_cast<int>(members.size() - 1) / 2;
            for (int k = 0; k < static_cast<int>(members.size()); ++k) {
                int offset = (k - mid) * spacing;
                if (offset == 0) {
                    continue;
                }
                auto& member = members[k];
                std::map<NudgeConn*, std::vector<RPoint>> originals;
                for (auto* seg : member.segments) {
                    if (!originals.count(seg->conn)) {
                        originals[seg->conn] = *seg->conn->points;
                    }
                }
                bool applied = false;
                for (int candidate : { offset, -offset }) {
                    std::vector<TouchedPoint> touched;
                    applyMemberOffset(member, candidate, touched);
                    if (validateMember(member, originals)) {
                        ++moved;
                        applied = true;
                        break;
                    }
                    undoOffsets(touched);
                }
                (void)applied;
            }
        }

        std::vector<ChannelSegment*> clustered;
        for (auto& cluster : channels) {
            for (auto* seg : cluster) {
                clustered.push_back(seg);
            }
        }
        std::vector<ChannelSegment*> singles;
        for (auto& seg : segments) {
            if (std::find(clustered.begin(), clustered.end(), &seg) == clustered.end()) {
                singles.push_back(&seg);
            }
        }
        std::sort(singles.begin(), singles.end(), [](ChannelSegment* a, ChannelSegment* b) {
            auto keyOf = [](ChannelSegment* s) {
                return std::string(1, s->orient) + ':' + std::to_string(s->track) + ':'
                    + std::to_string(s->lo) + ':' + s->connKey;
            };
            return keyOf(a) < keyOf(b);
        });

        const double INF = 1e18;
        for (auto* segPtr : singles) {
            auto& seg = *segPtr;
            double below = INF;
            double above = INF;
            for (const auto& r : rects) {
                if (seg.orient == 'H') {
                    if (seg.hi <= r.left || seg.lo >= r.right) {
                        continue;
                    }
                    if (r.bottom <= seg.track) {
                        above = (std::min)(above, seg.track - r.bottom);
                    }
                    if (r.top >= seg.track) {
                        below = (std::min)(below, r.top - seg.track);
                    }
                } else {
                    if (seg.hi <= r.top || seg.lo >= r.bottom) {
                        continue;
                    }
                    if (r.right <= seg.track) {
                        above = (std::min)(above, seg.track - r.right);
                    }
                    if (r.left >= seg.track) {
                        below = (std::min)(below, r.left - seg.track);
                    }
                }
            }
            double clearance = (std::min)(above, below);
            // Only act on block-hugging segments (closer than one grid cell).
            if (!(clearance < spacing)) {
                continue;
            }
            if (above >= INF && below >= INF) {
                continue;
            }

            double upEdge = (above < INF) ? seg.track - above : seg.track - 3.0 * spacing;
            double downEdge = (below < INF) ? seg.track + below : seg.track + 3.0 * spacing;
            int target
                = static_cast<int>(std::llround((upEdge + downEdge) / 2.0 / spacing)) * spacing;
            int offset = target - seg.track;
            if (offset == 0) {
                continue;
            }
            double newClearance = (std::min)((above < INF) ? above + offset : INF,
                (below < INF) ? below - offset : INF);
            if (!(newClearance > clearance)) {
                continue;
            }

            auto& pts = *seg.conn->points;
            char axis = (seg.orient == 'H') ? 'y' : 'x';
            // A guard bend only works when the motion is PERPENDICULAR to the
            // stub; skip otherwise (the path would fold back on itself).
            auto stubAxisOf
                = [](const RPoint& a, const RPoint& b) { return (a.y == b.y) ? 'x' : 'y'; };
            if (seg.segIndex == 1 && stubAxisOf(pts[0], pts[1]) == axis) {
                continue;
            }
            if (seg.segIndex == static_cast<int>(pts.size()) - 3
                && stubAxisOf(pts[pts.size() - 2], pts[pts.size() - 1]) == axis) {
                continue;
            }

            std::vector<RPoint> original = pts;
            int i = seg.segIndex;
            if (i == 1) {
                pts.insert(pts.begin() + 1, pts[1]);
                i += 1;
            }
            if (i == static_cast<int>(pts.size()) - 3) {
                pts.insert(pts.end() - 2, pts[pts.size() - 2]);
            }
            if (axis == 'y') {
                pts[i].y += offset;
                pts[i + 1].y += offset;
            } else {
                pts[i].x += offset;
                pts[i + 1].x += offset;
            }

            int track = (seg.orient == 'H') ? pts[i].y : pts[i].x;
            int lo = (seg.orient == 'H') ? (std::min)(pts[i].x, pts[i + 1].x)
                                         : (std::min)(pts[i].y, pts[i + 1].y);
            int hi = (seg.orient == 'H') ? (std::max)(pts[i].x, pts[i + 1].x)
                                         : (std::max)(pts[i].y, pts[i + 1].y);
            bool ok = stubsIntact(*seg.conn, original, spacing);
            if (ok) {
                for (const auto& r : rects) {
                    if (segmentCrossesRect(seg.orient, track, lo, hi, r)) {
                        ok = false;
                        break;
                    }
                }
            }
            if (ok) {
                for (const auto& other : segments) {
                    if (other.conn == seg.conn || other.sourceKey == seg.sourceKey
                        || other.orient != seg.orient) {
                        continue;
                    }
                    int otrack, olo, ohi;
                    currentGeom(other, otrack, olo, ohi);
                    if (otrack != track) {
                        continue;
                    }
                    if ((std::min)(hi, ohi) - (std::max)(lo, olo) > 0) {
                        ok = false;
                        break;
                    }
                }
            }
            if (ok) {
                ++moved;
                break; // one centering correction per pass (indices shifted)
            }
            *seg.conn->points = original;
        }

        return moved;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
