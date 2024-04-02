use std::collections::BTreeMap;

pub mod csl_adapter;
pub mod key_wallet;
pub mod ogmios;
pub mod plutip;
pub mod script;

pub mod from_csl;
pub mod to_csl;

pub fn union_b_tree_maps_with<const N: usize, K: Clone + Ord, V: Clone, F: Fn(&V, &V) -> V>(
    f: F,
    maps: [&BTreeMap<K, V>; N],
) -> BTreeMap<K, V> {
    maps.into_iter().fold(BTreeMap::new(), |acc, m| {
        m.iter().fold(acc, |mut acc, (k, v)| {
            acc.entry(k.clone())
                .and_modify(|va: &mut V| *va = f(va, v))
                .or_insert(v.clone());

            acc
        })
    })
}
