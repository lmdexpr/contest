#!/usr/bin/env python3
"""
Analyze 100 test case results from the score file to identify patterns and optimization opportunities.
"""

import re
import statistics
from collections import defaultdict

def parse_score_file(filename):
    """Parse the score file and extract test case data."""
    data = []
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        
        # Look for parameter lines
        if line.startswith('n = '):
            params_match = re.search(r'n = (\d+), k = (\d+), h = (\d+), t = (\d+), d = (\d+)', line)
            if params_match:
                n, k, h, t, d = map(int, params_match.groups())
                
                # Look for score line
                if i + 1 < len(lines):
                    score_line = lines[i + 1].strip()
                    score_match = re.search(r'score= ([\d.]+)', score_line)
                    if score_match:
                        score = float(score_match.group(1))
                        data.append({
                            'n': n, 'k': k, 'h': h, 't': t, 'd': d, 'score': score
                        })
        i += 1
    
    return data

def get_cluster_count(d):
    """Get the cluster count for a given d value according to current algorithm."""
    if d <= 100:
        return 30
    elif d <= 500:
        return 25
    elif d <= 1000:
        return 20
    elif d <= 3000:
        return 15
    else:
        return 8

def analyze_data(data):
    """Analyze the test data and provide insights."""
    print(f"Total test cases: {len(data)}")
    print(f"Score range: {min(d['score'] for d in data):.0f} - {max(d['score'] for d in data):.0f}")
    print(f"Average score: {statistics.mean(d['score'] for d in data):.0f}")
    print()
    
    # Analyze by d value ranges
    print("=== Analysis by d (cost) value ===")
    d_ranges = [
        (0, 100, "d ≤ 100"),
        (101, 500, "100 < d ≤ 500"),
        (501, 1000, "500 < d ≤ 1000"),
        (1001, 3000, "1000 < d ≤ 3000"),
        (3001, 10000, "d > 3000")
    ]
    
    for min_d, max_d, label in d_ranges:
        filtered = [d for d in data if min_d <= d['d'] <= max_d]
        if filtered:
            scores = [d['score'] for d in filtered]
            cluster_counts = [get_cluster_count(d['d']) for d in filtered]
            print(f"{label}: {len(filtered)} cases")
            print(f"  Avg score: {statistics.mean(scores):.0f}")
            print(f"  Min score: {min(scores):.0f}")
            print(f"  Max score: {max(scores):.0f}")
            print(f"  Cluster count: {cluster_counts[0] if cluster_counts else 'N/A'}")
            print()
    
    # Analyze by k value
    print("=== Analysis by k (tube count) value ===")
    k_stats = defaultdict(list)
    for d in data:
        k_stats[d['k']].append(d['score'])
    
    for k in sorted(k_stats.keys()):
        scores = k_stats[k]
        print(f"k = {k}: {len(scores)} cases, avg score: {statistics.mean(scores):.0f}")
    print()
    
    # Find best and worst cases
    print("=== Best performing cases (score < 250000) ===")
    best_cases = [d for d in data if d['score'] < 250000]
    best_cases.sort(key=lambda x: x['score'])
    
    for case in best_cases[:10]:  # Top 10
        cluster_count = get_cluster_count(case['d'])
        print(f"Score: {case['score']:.0f}, k={case['k']}, d={case['d']}, clusters={cluster_count}")
    print()
    
    print("=== Worst performing cases (score > 800000) ===")
    worst_cases = [d for d in data if d['score'] > 800000]
    worst_cases.sort(key=lambda x: x['score'], reverse=True)
    
    for case in worst_cases[:10]:  # Worst 10
        cluster_count = get_cluster_count(case['d'])
        print(f"Score: {case['score']:.0f}, k={case['k']}, d={case['d']}, clusters={cluster_count}")
    print()
    
    # Analyze correlation between parameters and score
    print("=== Parameter correlation analysis ===")
    
    # Low d, high k cases
    low_d_high_k = [d for d in data if d['d'] <= 100 and d['k'] >= 15]
    if low_d_high_k:
        scores = [d['score'] for d in low_d_high_k]
        print(f"Low d (≤100) + High k (≥15): {len(low_d_high_k)} cases, avg score: {statistics.mean(scores):.0f}")
    
    # High d, low k cases
    high_d_low_k = [d for d in data if d['d'] >= 1000 and d['k'] <= 8]
    if high_d_low_k:
        scores = [d['score'] for d in high_d_low_k]
        print(f"High d (≥1000) + Low k (≤8): {len(high_d_low_k)} cases, avg score: {statistics.mean(scores):.0f}")
    print()
    
    # Analyze cluster efficiency
    print("=== Current cluster count analysis ===")
    cluster_efficiency = defaultdict(list)
    for d in data:
        cluster_count = get_cluster_count(d['d'])
        cluster_efficiency[cluster_count].append(d['score'])
    
    for cluster_count in sorted(cluster_efficiency.keys()):
        scores = cluster_efficiency[cluster_count]
        print(f"Clusters = {cluster_count}: {len(scores)} cases, avg score: {statistics.mean(scores):.0f}")
    print()
    
    # Recommendations
    print("=== Recommendations ===")
    
    # Check if more clusters help with low d values
    low_d_cases = [d for d in data if d['d'] <= 100]
    if low_d_cases:
        low_d_scores = [d['score'] for d in low_d_cases]
        print(f"Low d cases (current 30 clusters): avg {statistics.mean(low_d_scores):.0f}")
        print("→ Consider increasing clusters for low d values (30→35 or 40)")
    
    # Check if fewer clusters help with high d values
    high_d_cases = [d for d in data if d['d'] >= 3000]
    if high_d_cases:
        high_d_scores = [d['score'] for d in high_d_cases]
        print(f"High d cases (current 8 clusters): avg {statistics.mean(high_d_scores):.0f}")
        print("→ Consider fewer clusters for very high d values (8→5 or 6)")
    
    # Analyze k-dependent patterns
    high_k_cases = [d for d in data if d['k'] >= 15]
    low_k_cases = [d for d in data if d['k'] <= 6]
    if high_k_cases and low_k_cases:
        high_k_avg = statistics.mean(d['score'] for d in high_k_cases)
        low_k_avg = statistics.mean(d['score'] for d in low_k_cases)
        print(f"High k (≥15) avg score: {high_k_avg:.0f}")
        print(f"Low k (≤6) avg score: {low_k_avg:.0f}")
        if high_k_avg < low_k_avg:
            print("→ High k values perform better - consider k-dependent cluster adjustment")

if __name__ == "__main__":
    data = parse_score_file("score")
    analyze_data(data)