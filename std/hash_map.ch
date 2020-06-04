let default_bucket_size: Int = 8.
let threshold: Float = 0.75.

object HashMap<_K, _V> {
  size: Int,

  buckets: [Bucket<_K, _V>],
  num_buckets: Int,
}

impl<_K, _V> for HashMap<_K, _V> {
  fn new() -> HashMap<_K, _V> {
    allocate HashMap {
      size: 0,

      buckets: allocate [Bucket; default_bucket_size],
      num_buckets: default_bucket_size
    }
  }

  fn bucket_from_hash(self, hash: Int) -> Bucket<_K, _V> {
    self:buckets[abs(hash % self:num_buckets)]
  }
}

impl<_K, _V> for HashMap<_K, _V> where _K: Equals<_K> + Hash {
  fn get(self, key: _K) -> Option<_V> {
    let hash = key:hash().

    for (maybe_hash, maybe_key, maybe_value) in self:bucket_from_hash(hash):entries {
      if hash == maybe_hash {
        if key == maybe_key {
          return Option!Some(maybe_value).
        }
      }
    }

    Option!None
  }

  fn remove(self, key: _K) -> Option<_V> {
    let hash = key:hash().
    let bucket = self:bucket_from_hash(hash).

    let next_link = bucket:entries:root.
    while true {
      match next_link {
        Option!Some(link) => {
          let (old_hash, old_key, old_value) = link:get().

          if hash == old_hash {
            if key == old_key {
              link:unlink().
              self:size = self:size - 1.
              return Option!Some(old_value).
            }
          }

          next_link = link:next.
        },
        Option!None => {
          break.
        },
      }
    }

    Option!None
  }

  fn put(self, key: _K, value: _V) -> Option<_V> {
    let hash = key:hash().
    let bucket = self:bucket_from_hash(hash).

    let next_link = bucket:entries:root.
    while true {
      match next_link {
        Option!Some(link) => {
          let (old_hash, old_key, old_value) = link:get().

          if hash == old_hash {
            if key == old_key {
              link:set((hash, key, value)).
              return Option!Some(old_value).
            }
          }

          next_link = link:next.
        },
        Option!None => {
          break.
        },
      }
    }

    self:size = self:size + 1.
    bucket:entries:push_back((hash, key, value)).

    self:try_grow().
    Option!None
  }

  fn try_grow(self) {
    if (self:size as Float) > (self:num_buckets as Float) * threshold {
      let old_buckets = self:buckets.
      self:num_buckets = self:num_buckets * 2.
      self:buckets = allocate [Bucket; self:num_buckets].

      for bucket in old_buckets {
        for (hash, key, value) in bucket:entries {
          self:bucket_from_hash(hash):entries:push_back((hash, key, value)).
        }
      }
    }
  }
}

impl<_K, _V> Len for HashMap<_K, _V> {
  fn len(self) -> Int = self:size.
}

impl<_K, _V> Deref<_K> for HashMap<_K, _V> where _K: Equals<_K> + Hash {
  type Result = _V.

  fn deref(self, idx: _K) -> _V {
    match self:get(idx) {
      Option!Some(v) => v,
      Option!None => panic("No such index"),
    }
  }
}

impl<_K, _V> DerefAssign<_K> for HashMap<_K, _V> where _K: Equals<_K> + Hash {
  type Value = _V.

  fn deref_assign(self, idx: _K, value: _V) -> _V {
    self:put(idx, value).
    value
  }
}

impl<_K, _V> Iterable for HashMap<_K, _V> {
  type Iterator = HashMapIterator<_K, _V>.
  type Item = (_K, _V).

  fn iterator(self) -> HashMapIterator<_K, _V> {
    let (next_bucket, buckets) = self:buckets:iterator():next().

    HashMapIterator!Iterator {
      size_hint: self:size,
      buckets,
      links: next_bucket:unwrap():entries:iterator()
    }
  }
}

enum HashMapIterator<_K, _V> {
  Iterator {
    size_hint: Int,
    buckets: ArrayIterator<Bucket<_K, _V>>,
    links: ListIterator<(Int, _K, _V)>,
  },
}

impl<_K, _V> Iterator for HashMapIterator<_K, _V> {
  type Item = (_K, _V).

  fn next(self) -> (Option<(_K, _V)>, HashMapIterator<_K, _V>) {
    let HashMapIterator!Iterator { size_hint, buckets, links } = self.

    if links:has_next() {
      let (next_link, links) = links:next().
      let (_, k, v) = next_link:unwrap().
      return (Option!Some((k, v)), HashMapIterator!Iterator { size_hint: size_hint - 1, buckets, links }).
    }

    let ArrayIterator!Iterator { idx: buckets_idx, ... } = buckets.

    while buckets:has_next() {
      let (next_bucket, next_buckets) = buckets:next().
      buckets = next_buckets. // We can't overwrite in a destructure.

      let next_bucket = next_bucket:unwrap().

      if next_bucket:entries:len() > 0 {
        let (next_link, links) = next_bucket:entries:iterator():next().
        let (_, k, v) = next_link:unwrap().

        return (Option!Some((k, v)), HashMapIterator!Iterator { size_hint: size_hint - 1, buckets, links }).
      }
    }

    assert size_hint == 0.
    (Option!None, HashMapIterator!Iterator { size_hint, buckets, links })
  }

  fn has_next(self) -> Bool {
    let HashMapIterator!Iterator { size_hint, ... } = self.
    size_hint > 0
  }

  fn size_hint(self) -> Int {
    let HashMapIterator!Iterator { size_hint, ... } = self.
    size_hint
  }
}

impl<_K, _V> Into<String> for HashMap<_K, _V> where _K: Into<String>, _V: Into<String> {
  fn into(self) -> String {
      let s = "HashMap{".
      let first = true.

      for (k, v) in self {
          if first {
              first = false.
          } else {
              s = s + ", ".
          }

          s = s + (k as String) + ": " + (v as String).
      }

      s + "}"
  }
}

object Bucket<_K, _V> {
  entries: List<(Int, _K, _V)>,
}

impl<_K, _V> Default for Bucket<_K, _V> {
  fn default() -> Bucket<_K, _V> =
    allocate Bucket { entries: List:new() }.
}

impl<_K, _V> Into<String> for Bucket<_K, _V> where _K: Into<String>, _V: Into<String> {
  fn into(self) -> String = "Bucket { \(self:entries) }".
}
