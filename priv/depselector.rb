require 'rubygems'
require 'dep_selector'
require 'erlectricity'

def translate_constraint(constraint)
  case constraint
  when Symbol
    case constraint
    when :gt then ">"
    when :gte then ">="
    when :lt then "<"
    when :lte then "<="
    when :eq then "="
    when :pes then "~>"
    else constraint.to_s
    end
  when NilClass
    "="
  else
    constraint
  end
end

receive do |m|
  m.when([:solve, Erl.hash]) do |data|
    # create dependency graph from cookbooks
    graph = DepSelector::DependencyGraph.new

    data[:filtered_versions].each do |vsn|
      name, version_constraints = vsn
      version_constraints.each do |version_constraint| # todo: constraints become an array in ruby
                                                       # due to the erlectricity conversion from
                                                       # tuples
        version, constraints = version_constraint
        package_version = graph.package(name).add_version(DepSelector::Version.new(version))
        constraints.each do |package_constraint|
          constraint_name, constraint_version, constraint = package_constraint
          constraint_string = "#{translate_constraint(constraint)} #{constraint_version}"
          version_constraint = DepSelector::VersionConstraint.new(constraint_string)
          dependency = DepSelector::Dependency.new(graph.package(constraint_name), version_constraint)
          package_version.dependencies << dependency
        end
      end
    end

    all_versions = data[:all_versions].inject([]) do |acc, (name, constriants)|
      acc << graph.package(name)
    end

    run_list = data[:run_list].map do |run_list_item|
      case run_list_item
      when Array
        item_name, item_constraint = run_list_item
        version_constraint = DepSelector::VersionConstraint.new(item_constraint)
        DepSelector::SolutionConstraint.new(graph.package(item_name), version_constraint)
      else
        version_constraint = DepSelector::VersionConstraint.new(">= 0.0.0")
        DepSelector::SolutionConstraint.new(graph.package(run_list_item), version_constraint)
      end
    end

    selector = DepSelector::Selector.new(graph, 10)

    answer = begin
               solution = selector.find_solution(run_list, all_versions)
               packages = Erl::List.new
               solution.each do |package, v|
                 packages << [package, [v.major, v.minor, v.patch]]
               end
               [:ok, packages]
             rescue DepSelector::Exceptions::InvalidSolutionConstraints => e
               non_existent_cookbooks = e.non_existent_packages.inject(Erl::List.new) do |list, constraint|
                 list << constraint.package.name
               end

               constrained_to_no_versions = e.constrained_to_no_versions.inject(Erl::List.new) do |list, constraint|
                 list << constraint.to_s
               end

               error_detail = Erl::List.new([[:non_existent_cookbooks, non_existent_cookbooks],
                                             [:constraints_not_met, constrained_to_no_versions]])

               [:error, :invalid_constraints, error_detail]
             rescue DepSelector::Exceptions::NoSolutionExists => e
               most_constrained_cookbooks = e.disabled_most_constrained_packages.inject(Erl::List.new) do |list, package|
                 # WTF: this is the reported error format but I can't find this anywhere in the ruby code
                 list << "#{package.name} = #{package.versions.first.to_s}"
               end

               non_existent_cookbooks = e.disabled_non_existent_packages.inject(Erl::List.new) do |list, package|
                 list << package.name
               end

               error_detail = Erl::List.new([[:message, e.message],
                                             [:unsatisfiable_run_list_item, e.unsatisfiable_solution_constraint.to_s],
                                             [:non_existent_cookbooks, non_existent_cookbooks],
                                             [:most_constrained_cookbooks, most_constrained_cookbooks]])

               [:error, :no_solution, error_detail]
             rescue DepSelector::Exceptions::TimeBoundExceededNoSolution => e
               [:error, :timeout]
             end

    m.send!(answer)
    m.receive_loop
  end
end
