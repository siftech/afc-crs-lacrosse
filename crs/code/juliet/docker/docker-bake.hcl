variable REGISTRY {
  default = ""
}

variable ORGANIZATION {
  default = "lacrosse"
}

function "compose_tag" {
  params = [tag]
  result = notequal("", tag) ? ":${tag}" : ""
}

function "compose_image" {
  params = [name, tag]
  result = "${REGISTRY}/${ORGANIZATION}/${name}${compose_tag(tag)}"
}

group "default" {
  targets = ["juliet-java"]
}

target "infer" {
  context = ".."
  dockerfile = "./docker/infer-base.Dockerfile"
  tags = [compose_image("infer-base", "latest")]
}

target "juliet-java" {
  name = "juliet-java-${tgt}"
  matrix = {
    tgt = ["infer"]
  }
  context = ".."
  contexts = {
    compose_image("juliet-analysis-tool", "") = "target:${tgt}"
  }
  dockerfile = "./docker/juliet-java.Dockerfile"
  tags = [compose_image("juliet-java-${tgt}", "latest")]
}