// finding coordinates, where languages had been created
match
    (n:programming_language)-[:place]->(m:town)
    return replace(n.id, "_", " ") as language, n.year as year, replace(m.id, "_", " ") as town, m.lat as lat, m.lon as lon
  union
match
    (n:programming_language)-[:university|:company]->()-[:city|:hq]->(m:town)
    return replace(n.id, "_", " ") as language, n.year as year, replace(m.id, "_" ," ") as town, m.lat as lat, m.lon as lon
  union
match
    (n:programming_language)-[:designer]->()-[:job]->()-[:city|:hq]->(m:town)
    return replace(n.id, "_", " ") as language, n.year as year, replace(m.id, "_" , " ") as town, m.lat as lat, m.lon as lon

// clean database
match (n) detach delete n

// make index on the property
create index index_user_uid for (u:User) on (u.uid)

// show db schema
call db.schema.visualization
