docker pull apache/camel-jbang:4.4.0
docker run -v .:/integrations apache/camel-jbang:4.4.0 run /integrations/hello.java --prompt
docker run -p 11:11 -v .:/integrations apache/camel-jbang:4.4.0 run /integrations/http.java --prompt

camel run hello.java http.java standalone.java
camel run hello.java post_vote.java
camel run post_vote.java
camel run test.java
camel run post_emargement.yaml post_vote.yaml get_vote.yaml get_emargement.yaml get_vote_waiting.yaml
camel run get_vote_waiting.yaml

docker run -p 11:11 -v .:/integrations apache/camel-jbang:4.4.0 run /integrations/http.java /integrations/hello.java --prompt
docker run -p 11:11 -v .:/integrations apache/camel-jbang:4.4.0 run /integrations/post_vote.java --prompt



 



