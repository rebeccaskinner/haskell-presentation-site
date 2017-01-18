#!/bin/bash

# Create a new post
http post 'locahost:3000/createpost?title="title1"&contents="content1"'

# List posts
http get localhost:3000/posts

# Show our post
http get localhost:3000/posts/1

# Upvote our post
http post localhost:3000/posts/1?action="upvote"

# Downvote our post
http post localhost:3000/posts/1?action="downvote"
